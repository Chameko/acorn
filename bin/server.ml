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
  catch (fun () ->
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
    match msg with
    | Some msg ->
      let reply, cont, term = handle_message msg in
      Logs_lwt.app (fun m -> m "%s" reply)
      >>= (fun () -> Lwt_io.write_line oc reply)
      >>= (fun () -> 
      if cont then
        handle_connection ic oc ()
      else
        return_ok term)
    | None -> 
      Logs_lwt.info (fun m -> m "Connection closed")
      >>= (fun () -> return_ok @@ true)))
  (fun exn ->
    Logs_lwt.err (fun m -> m "Failed to read from client: %s" @@ Exn.to_string exn)
    >|= (fun () -> Error K_error.FailedToRecvClient))

(** Check if the server is running already *)
let is_server_running paths =
  Logs.debug (fun m -> m "Determining if server is already running");
  let read_pid_file () = In_channel.with_file (Paths.pid_file paths) ~f:(fun ic ->
    try
      Ok (Stdlib.String.trim @@ In_channel.input_all ic)
    with
    | Sys_error e ->
      Logs.err (fun m -> m "Failed to read PID file %s" e );
      Error K_error.FailedToReadFile)
  in
  let pid =
    try read_pid_file () with Sys_error e ->
      Logs.debug (fun m -> m "Failed to read PID file %s" e);
      Logs.debug (fun m -> m "Attempting to create PID file and try again");
      try Result.((create_pid paths) >>= read_pid_file) with Sys_error e ->
        Logs.err(fun m -> m "Failed to read PID file %s" e);
        Error K_error.FailedToReadFile
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
        Ok (String.equal (In_channel.input_all ic) "kakorn")
    with
    | Sys_error e ->
      Logs.err (fun m -> m "Failed to read command %s" e);
      Error K_error.FailedToReadFile
  in
  Result.(pid >>= comm)

(** Accept a connection *)
let accept_connection conn =
  let fd, _ = conn in
  let channels () =
    Logs_lwt.debug (fun m -> m "Accepting connection")
    >>= (fun () -> return_ok (
      Lwt_io.of_fd ~mode:Lwt_io.Input fd,
      Lwt_io.of_fd ~mode:Lwt_io.Output fd
    ))
  in
  let handle_connection_map ch =
    match ch with
    | Error e -> return_error e
    | Ok (ic, oc) ->
      handle_connection ic oc ()
      >>= (fun res -> return @@ Lwt_unix.shutdown fd Lwt_unix.SHUTDOWN_ALL
      >>= (fun () -> Lwt_unix.close fd)
      >>= (fun () -> return res))
  in
  catch channels
  (fun e ->
    Logs_lwt.err (fun m -> m "Failed creating channels for connection: %s" @@ Exn.to_string e)
    >>= (fun () -> Lwt_unix.close fd >>= fun () -> return_error K_error.FailedConnection))
  >>= handle_connection_map

(** Main server loop *)
let rec serve_loop sock () =
  let accept () =
    catch (fun () -> Lwt_unix.accept sock >>= return_ok)
    (fun exn ->
      Logs_lwt.err (fun m -> m "Failed to accept connection: %s" @@ Exn.to_string exn)
      >>= (fun () -> return_error K_error.FailedConnection))
  in
  let serve res =
    match res with
    | Error e -> return_error e
    | Ok true -> serve_loop sock ()
    | Ok false -> return_ok ()
  in
  accept ()
  >>= (fun res -> match res with Ok conn -> accept_connection conn | Error e -> return_error e)
  >>= serve

(** Start server *)
let start_server paths =
  (* Create dir *)
  match create_dir paths with
  | Error e ->
    Logs.err (fun m -> m "%s" @@ K_error.output e)
  | Ok () ->
    (* Check if the server is running *)
    match is_server_running paths with
    | Ok true -> Logs.err (fun m -> m "%s" @@ K_error.output K_error.ServerAlreadyRunning)
    | Error e -> Logs.err (fun m -> m "%s" @@ K_error.output e)
    | Ok false ->
      Logs.debug (fun m -> m "Running server");
      let promise =
        (* Create socket *)
        create_server_socket paths ()
        (* Serve the socket *)
        >>= (fun sock ->
          let run () = match sock with
          | Ok sock -> 
            (* Run server loop *)
            serve_loop sock ()
            (* Close socket file *)
            >>= (fun res -> Lwt_unix.shutdown sock Lwt_unix.SHUTDOWN_ALL; Lwt_unix.close sock >|= (fun () -> res))
          | Error e ->
            return_error e
          in
          catch run (fun exn ->
            Logs_lwt.err (fun m -> m "Fatal error: %s" @@ Exn.to_string exn)
            >>= (fun () -> return_ok ())
          ))
        >>= (fun res ->
          match res with
          | Ok () ->
            Logs_lwt.app (fun m -> m "Server closed...")
          | Error e ->
            Logs_lwt.err (fun m -> m "%s" @@ K_error.output e))
      in
      Lwt_main.run promise
