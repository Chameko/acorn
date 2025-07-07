open Base
open Lwt

type server =
  { paths : Resources.paths
  ; sessions : Kak.session list ref
  }

(** Send debug information to both the server and client *)
let server_client_debug msg oc =
  let%lwt () = Logs_lwt.debug (fun m -> m "%s" msg) in
  (* Write header so the client knows to send this to debug *)
  Command.Client.client_debug msg oc
;;

(** Reads the buffer from the FIFO pipe. Returns a promise and a resolver that will unblock this and clean up *)
let read_fifo (buffer : Kak.buffer) =
  try%lwt
    (* Read a line from the FIFO *)
    let%lwt line = Lwt_io.read_line_opt buffer.ic in
    match Option.bind line ~f:(fun l -> Int.of_string_opt (Stdlib.String.trim l)) with
    | Some bytes ->
      let%lwt () = Logs_lwt.debug (fun m -> m "Buffer recieved...") in
      (* Read the bytes *)
      let%lwt data = Lwt_io.read ~count:bytes buffer.ic in
      Logs_lwt.debug (fun m -> m "Buffer %s\n%s" buffer.name data)
    | None -> return_unit
  with
  | Unix.Unix_error (e, _, _) ->
    Logs_lwt.debug (fun m ->
      m "%s" (K_error.output (K_error.FileIOFailure (Unix.error_message e))))
;;

(** Opens a buffer FIFO *)
let open_buffer server (session : Kak.session) name oc =
  let fifo_name = Resources.buff_file server.paths session.id name in
  try%lwt
    (* Create the buf dir if it doesn't exist *)
    let%lwt () =
      try%lwt Lwt_unix.mkdir (Resources.buff_dir server.paths) 0o755 with
      (* If the directory already exits thats fine *)
      | Unix.Unix_error (EEXIST, _, _) ->
        Logs_lwt.debug (fun m -> m "Using existing buffer directory")
    in
    (* Make the FIFO for communication *)
    let%lwt () = Lwt_unix.mkfifo fifo_name 0o666 in
    (* Open the FIFO for reading *)
    let%lwt ic =
      Lwt_io.open_file
        ~flags:[ Lwt_unix.O_RDONLY; Lwt_unix.O_NONBLOCK ]
        ~mode:Lwt_io.Input
        fifo_name
    in
    let%lwt () = Kak.kak_sock_send server.paths session "echo -debug Epic" in
    (* Inform kakoune of the buffer *)
    let%lwt () = Command.Client.client_output fifo_name oc in
    let open Kak in
    let buffer = { name; path = fifo_name; ic } in
    session.buffers := buffer :: !(session.buffers);
    return_unit
  with
  | Unix.Unix_error (e, _, _) ->
    Logs_lwt.debug (fun m ->
      m "%s"
      @@ K_error.output
           (K_error.FileIOFailure
              ("Failed to create FIFO " ^ fifo_name ^ ": " ^ Unix.error_message e)))
;;

let init_session server cmd oc =
  let session = Kak.kak_session cmd server.paths in
  if Option.is_none @@ List.find !(server.sessions) ~f:(fun s -> Int.equal s.id cmd)
  then (
    let () = server.sessions := session :: !(server.sessions) in
    let%lwt () = server_client_debug ("Initializing session " ^ Int.to_string cmd) oc in
    let%lwt () = Kak.kak_sock_send server.paths session "echo -debug Hello kakoune" in
    let%lwt () =
      Lwt_io.write_line
        oc
        (Command.Client.Output (String.length Rc.init)
         |> Command.Client.sexp_of_reply
         |> Sexp.to_string)
    in
    Lwt_io.write oc Rc.init)
  else server_client_debug "Session already exists" oc
;;

let handle_message server msg oc =
  let open Sexplib in
  let open Command in
  (* Process request *)
  match Sexp.of_string_conv msg Server.command_of_sexp with
  | `Result Term ->
    let%lwt () = Logs_lwt.debug (fun m -> m "Terminating...") in
    Lwt.return_false
  | `Result (OpenBuffer cmd) ->
    (match List.find ~f:(fun s -> Int.equal cmd.session s.id) !(server.sessions) with
     | Some s ->
       let%lwt () = open_buffer server s cmd.name oc in
       let%lwt () = server_client_debug "Opened buffer" oc in
       Lwt.return_true
     | None ->
       let%lwt () = server_client_debug "Session not registered" oc in
       Lwt.return_false)
  | `Result (CloseBuffer cmd) -> Lwt.return_true
  | `Result (Init cmd) ->
    let%lwt () = init_session server cmd oc in
    Lwt.return_true
  | `Error (exn, _) ->
    let%lwt () = server_client_debug ("Bad request: " ^ Exn.to_string exn) oc in
    Lwt.return_true
;;

(** Handle the connection *)
let rec handle_connection server ic oc =
  try%lwt
    (* Recieve message *)
    let%lwt msg = Lwt_io.read_line_opt ic in
    (* Process message *)
    match msg with
    | Some msg ->
      let%lwt cont = handle_message server msg oc in
      (* Inform the client we've sent all we need to *)
      let%lwt () =
        Lwt_io.write_line
          oc
          (Command.Client.End |> Command.Client.sexp_of_reply |> Sexp.to_string)
      in
      if not cont then return_ok cont else handle_connection server ic oc
    | None ->
      (* Connection terminated *)
      let%lwt () = Logs_lwt.debug (fun m -> m "Connection closed") in
      return_ok true
  with
  | Unix.Unix_error (e, _, _) ->
    return_error
    @@ K_error.SocketIOFailure
         ("Failed to communicate with client: " ^ Unix.error_message e)
;;

(** Check if the server is running already *)
let is_server_running server =
  Logs.debug (fun m -> m "Determining if server is already running");
  (* Function for reading the PID file *)
  let read_pid_file () =
    Stdio.In_channel.with_file (Resources.pid_file server.paths) ~f:(fun ic ->
      try Ok (Stdlib.String.trim @@ In_channel.input_all ic) with
      | Sys_error e -> Error (K_error.FileIOFailure ("Failed to read PID file: " ^ e)))
  in
  (* Get the pid *)
  let pid =
    try read_pid_file () with
    | Sys_error e ->
      Logs.debug (fun m -> m "Failed to read PID file: %s" e);
      Logs.debug (fun m -> m "Attempting to create PID file and trying again");
      (try Result.(Resources.create_pid server.paths >>= read_pid_file) with
       | Sys_error e ->
         Logs.err (fun m -> m "Failed to read PID file %s" e);
         Error (K_error.FileIOFailure ("Failed to read PID file: " ^ e)))
  in
  (* Check which process the PID belongs to *)
  let comm pid =
    let ic = Unix.open_process_in ("ps -p " ^ pid ^ " -o comm=") in
    try
      if String.equal (Int.to_string @@ Unix.getpid ()) pid
      then
        (* If the PID is us then we're all good :) *)
        Ok false
      else (
        (* Check if another server is running *)
        let output = Stdlib.String.trim @@ In_channel.input_all ic in
        Ok (String.equal output "kakorn"))
    with
    | Sys_error e ->
      Error (K_error.CommandIOFailure ("Failed to read from command: " ^ e))
  in
  Result.(pid >>= comm)
;;

(** Accept a connection *)
let accept_connection server sock =
  let%lwt fd, _ = Lwt_unix.accept sock in
  Lwt.finalize
    (fun () ->
       try%lwt
         let%lwt () = Logs_lwt.debug (fun m -> m "Accepting connection") in
         let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
         let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
         handle_connection server ic oc
       with
       | Unix.Unix_error (e, _, _) ->
         return_error
         @@ K_error.SocketIOFailure
              ("Failed to establish client I/O channels: " ^ Unix.error_message e))
    (fun () -> Lwt_unix.close fd)
;;

(** Check if a FIFO was written to and then read it *)
let check_buf server =
  (* Sleep to prevent busywaiting *)
  let%lwt () = Lwt_unix.sleep 0.1 in
  let%lwt () =
    Lwt.join
    @@ List.fold !(server.sessions) ~init:[] ~f:(fun buffers s ->
      List.append buffers (List.map !(s.buffers) ~f:(fun b -> read_fifo b)))
  in
  return_ok ()
;;

(** Main server loop *)
let serve_loop server sock =
  try%lwt
    (* Accept connections *)
    let rec accept_loop () =
      match%lwt accept_connection server sock with
      | Ok true -> accept_loop ()
      | Ok false -> return_ok ()
      | Error e -> return_error e
    in
    (* Keep checking fifos while we wait for connections *)
    let rec check_buf_loop () =
      match%lwt check_buf server with
      | Ok () -> check_buf_loop ()
      | Error e -> return_error e
    in
    Lwt.choose [ check_buf_loop (); accept_loop () ]
  with
  | Unix.Unix_error (e, _, _) ->
    return_error
    @@ K_error.SocketIOFailure ("Failed to accept connection: " ^ Unix.error_message e)
;;

(** Start server *)
let start_server server =
  (* Create dir *)
  match Resources.create_dir server.paths with
  | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
  | Ok () ->
    (* Check if the server is running *)
    (match is_server_running server with
     (* No error and server running  *)
     | Ok true ->
       Logs_lwt.err (fun m -> m "%s" @@ K_error.output K_error.ServerAlreadyRunning)
     (* Error  *)
     | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
     (* No error and no other server *)
     | Ok false ->
       (* Create PID file *)
       (match Resources.create_pid server.paths with
        | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
        | Ok () ->
          let%lwt () = Logs_lwt.debug (fun m -> m "Creating server socket") in
          (* Create socket *)
          let%lwt sock = Resources.create_server_socket server.paths () in
          (* Serve the socket *)
          let%lwt res =
            match sock with
            | Ok sock ->
              Lwt.finalize
                (fun () ->
                   (* Run server loop *)
                   let%lwt () = Logs_lwt.debug (fun m -> m "Running server...") in
                   serve_loop server sock)
                (fun () ->
                   (* Clean up the buffer FIFOs*)
                   let%lwt () =
                     Lwt.join
                     @@ List.fold !(server.sessions) ~init:[] ~f:(fun tasks s ->
                       List.append
                         tasks
                         (List.map !(s.buffers) ~f:(fun b ->
                            let%lwt () =
                              Logs_lwt.debug (fun m ->
                                m "Cleaning up %s... at %s" b.name b.path)
                            in
                            let%lwt () = Lwt_io.close b.ic in
                            Lwt_unix.unlink b.path)))
                   in
                   (* Clean up socket *)
                   Lwt_unix.close sock)
            | Error e -> return_error e
          in
          (match res with
           | Ok () -> return_unit
           | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e))))
;;
