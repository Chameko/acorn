open Core
open Base
open Lwt
open Resources

let counter = ref 0

(** Message handling *)
let handle_message msg =
  match msg with
  | "term" -> ("Exiting and closing server", false, false)
  | "exit" -> ("Exiting", false, true)
  | "read" -> (Int.to_string !counter, true, true)
  | "inc" -> counter := !counter + 1; ("Counter has been incramented", true, true)
  | _ -> ("Unknown Command", true, true)

(** Handle the connection *)
let rec handle_connection ic oc () =
  try%lwt
    (* Recieve message *)
    let%lwt msg = Lwt_io.read_line_opt ic in
    
    (* Process message *)
    match msg with
    | Some msg ->
      let reply, cont, term = handle_message msg in
      (* Output and reply to message *)
      let%lwt () = Logs_lwt.app (fun m -> m "%s" reply) in
      let%lwt () = Lwt_io.write_line oc reply in
      (* Determine to continue *)
      if cont then
        handle_connection ic oc ()
      else
        return_ok term
    | None -> 
      (* Connection terminated *)
      let%lwt () = Logs_lwt.info (fun m -> m "Connection closed") in
      return_ok @@ true
  with
    | Core_unix.Unix_error (e, _, _) ->
      return_error @@ K_error.SocketIOFailure ("Failed to communicate with client: " ^ Core_unix.Error.message e)

(** Check if the server is running already *)
let is_server_running paths =
  Logs.debug (fun m -> m "Determining if server is already running");
  (* Function for reading the PID file *)
  let read_pid_file () = In_channel.with_file (Paths.pid_file paths) ~f:(fun ic ->
    try
      Ok (Stdlib.String.trim @@ In_channel.input_all ic)
    with
    | Sys_error e ->
      Error (K_error.FileIOFailure ("Failed to read PID file: " ^ e)))
  in

  (* Get the pid *)
  let pid =
    try
      read_pid_file () 
    with
      Sys_error e ->
      Logs.debug (fun m -> m "Failed to read PID file: %s" e);
      Logs.debug (fun m -> m "Attempting to create PID file and trying again");
      try
        Result.(create_pid paths >>= read_pid_file)
      with 
      | Sys_error e ->
        Logs.err(fun m -> m "Failed to read PID file %s" e);
        Error (K_error.FileIOFailure ("Failed to read PID file: " ^ e))
  in

  (* Check which process the PID belongs to *)
  let comm pid =
    let ic =
      Core_unix.open_process_in ("ps -p " ^ pid ^ " -o comm=")
    in
    try
      if String.equal (Core.Pid.to_string @@ Core_unix.getpid ()) pid then
        (* If the PID is us then we're all good :) *)
        Ok(false)
      else
        (* Check if another server is running *)
        let output = Stdlib.String.trim @@ In_channel.input_all ic in
        Ok (String.equal (output) "kakorn")
    with
    | Sys_error e ->
      Error (K_error.CommandIOFailure ("Failed to read from command: " ^ e))
  in
  Result.(pid >>= comm)

(** Accept a connection *)
let accept_connection conn =
  let fd, _ = conn in

  (try%lwt
    let%lwt () = Logs_lwt.debug (fun m -> m "Accepting connection") in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    handle_connection ic oc ()
  with
  | Core_unix.Unix_error (e, _, _) ->
    return_error @@ K_error.SocketIOFailure ("Failed to establish client I/O channels: " ^ Core_unix.Error.message e))
  [%lwt.finally
    Lwt_unix.close fd]

(** Main server loop *)
let rec serve_loop sock () =
  let serve res =
    match res with
    | Error e -> return_error e
    | Ok true -> serve_loop sock ()
    | Ok false -> return_ok ()
  in

  try%lwt
    let%lwt conn = Lwt_unix.accept sock in
    let%lwt res = accept_connection conn in
    serve res
  with
  | Core_unix.Unix_error (e, _, _) ->
    return_error @@ K_error.SocketIOFailure ("Failed to accept connection: " ^ Core_unix.Error.message e)

(** Start server *)
let start_server paths =
  (* Create dir *)
  match create_dir paths with
  | Error e ->
    Logs.err (fun m -> m "%s" @@ K_error.output e)
  | Ok () ->
    (* Check if the server is running *)
    match is_server_running paths with
    (* No error and server running  *)
    | Ok true -> Logs.err (fun m -> m "%s" @@ K_error.output K_error.ServerAlreadyRunning)
    (* Error  *)
    | Error e -> Logs.err (fun m -> m "%s" @@ K_error.output e)
    (* No error and no other server *)
    | Ok false ->
      (* Create PID file *)
      match create_pid paths with
      | Error e -> Logs.err (fun m -> m "%s" @@ K_error.output e)
      | Ok () ->
        let promise =
          let%lwt () = Logs_lwt.debug (fun m -> m "Running server") in
          (* Create socket *)
          let%lwt sock = create_server_socket paths () in
          (* Serve the socket *)
          let%lwt res = match sock with
          | Ok sock -> 
            (* Run server loop *)
            serve_loop sock ()
          | Error e ->
            return_error e
          in
          match res with
          | Ok () -> return_unit
          | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
        in
        Lwt_main.run promise

