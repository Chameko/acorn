open Base
open Lwt

type server =
  { paths : Resources.paths
  ; sessions : Kak.session array ref
  }

let open_buffer (session : Kak.session) name =
  Kak.kak_oc session (fun oc ->
    Lwt_io.write_line oc#stdin ("echo -debug [Kakorn] Opened buffer " ^ name))
;;

let init_session server cmd oc =
  match%lwt Kak.kak_session cmd with
  | Ok session ->
    server.sessions := Array.append !(server.sessions) [| session |];
    Logs_lwt.debug (fun m -> m "Added session kak %d" cmd)
  | Error e -> Lwt_io.write_line oc (K_error.output e)
;;

let handle_message server msg oc =
  let open Sexplib in
  let open Command in
  (* Send debug to both client and server *)
  let server_client_debug msg =
    let%lwt () = Logs_lwt.debug (fun m -> m "%s" msg) in
    Lwt_io.write_line oc msg
  in
  (* Process request *)
  match Sexp.of_string_conv msg command_of_sexp with
  | `Result Term ->
    let%lwt () = server_client_debug "Terminating..." in
    Lwt.return_false
  | `Result (OpenBuffer cmd) ->
    (match Array.find ~f:(fun s -> Int.equal cmd.session s.id) !(server.sessions) with
     | Some s ->
       let%lwt () = open_buffer s cmd.name in
       let%lwt () = server_client_debug "Opened buffer" in
       Lwt.return_true
     | None ->
       let%lwt () = server_client_debug "Session not registered" in
       Lwt.return_false)
  | `Result (CloseBuffer cmd) -> Lwt.return_true
  | `Result (Init cmd) ->
    let%lwt () = init_session server cmd oc in
    Lwt.return_true
  | `Error (exn, _) ->
    let%lwt () = server_client_debug ("Bad request: " ^ Exn.to_string exn) in
    Lwt.return_true
;;

(** Handle the connection *)
let handle_connection server ic oc =
  try%lwt
    (* Recieve message *)
    let%lwt msg = Lwt_io.read_line_opt ic in
    (* Process message *)
    match msg with
    | Some msg ->
      let%lwt term = handle_message server msg oc in
      return_ok term
    | None ->
      (* Connection terminated *)
      let%lwt () = Logs_lwt.info (fun m -> m "Connection closed") in
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
let accept_connection server conn =
  let fd, _ = conn in
  (try%lwt
     let%lwt () = Logs_lwt.debug (fun m -> m "Accepting connection") in
     let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
     let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
     handle_connection server ic oc
   with
   | Unix.Unix_error (e, _, _) ->
     return_error
     @@ K_error.SocketIOFailure
          ("Failed to establish client I/O channels: " ^ Unix.error_message e))
    [%lwt.finally Lwt_unix.close fd]
;;

(** Main server loop *)
let rec serve_loop server sock =
  let serve res =
    match res with
    | Error e -> return_error e
    | Ok true -> serve_loop server sock
    | Ok false -> return_ok ()
  in
  try%lwt
    let%lwt conn = Lwt_unix.accept sock in
    let%lwt res = accept_connection server conn in
    serve res
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
              (* Run server loop *)
              let%lwt () = Logs_lwt.debug (fun m -> m "Running server...") in
              serve_loop server sock
            | Error e -> return_error e
          in
          (match res with
           | Ok () -> return_unit
           | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e))))
;;
