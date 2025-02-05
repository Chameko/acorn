open Paths
open Lwt
open Core
open Core_unix

(** Create the pid file *)
let create_pid paths =
  let open Result in
  Logs.debug (fun m -> m "Creating PID file");
  let pid_file = pid_file paths in
  (* Create out channel to file (creates file if needed )*)
  let oc =
    try Ok (Out_channel.create pid_file ?append:(Some false))
    with Sys_error e ->
      let () = Logs.err (fun m -> m "Failed to open PID file: %s" @@ e) in
      Error (K_error.FailedToCreateFiles)
  in
  (* Write the PID *)
  let write_pid oc =
    Exn.protect ~f:(fun () ->
      try
        (* Read the file *)
        Ok (Out_channel.output_string oc (Pid.to_string @@ getpid ()))
      with Sys_error e ->
        let () = Logs.err (fun m -> m "Failed to write to PID file: %s" @@ e) in
        Error (K_error.FailedToCreateFiles))
    (* Close the out channel *)
    ~finally:(fun () -> Out_channel.close oc)
  in
  oc >>= write_pid

(** Create the directory *)
let create_dir paths =
  Logs.debug (fun m -> m "Creating kakorn directory");
  try
    (* Create the directory if needed *)
    Ok(Core_unix.mkdir paths.runtime_dir)
  with
  (* If the directory already exits thats fine *)
  | Unix_error (Error.EEXIST, _, _) ->
    Logs.debug (fun m -> m "Using existing runtime directory");
    Ok (())
  (* Report any other error *)
  | Unix_error (e, _, _) ->
    Logs.err (fun m -> m "Failed to create runtime directory: %s" @@ Error.message e);
    Error (K_error.FailedToCreateFiles)

(** Create socket *)
let create_socket () =
  let open Lwt_unix in
  let socket () = return @@ socket PF_UNIX SOCK_STREAM 0
  >>= (fun sock -> return_ok sock)
  in
  catch socket (fun exn ->
    Logs_lwt.err (fun m -> m "Failed to create socket: %s" @@ Exn.to_string exn)
    >>= (fun () -> return @@ Error(K_error.FailedToCreateSocket)))

(** Create a socket for the server *)
let create_server_socket paths () =
  let open Lwt_unix in
  let bind_and_listen sock =
    match sock with
    | Ok sock ->
      bind sock @@ ADDR_UNIX(socket_file paths)
      >>= fun () -> listen sock 10; return @@ Ok(sock)
    | Error e -> return @@ Error e
  in
  
  let sock = create_socket () in

  catch (fun () ->
    sock >>= bind_and_listen)
  (fun exn ->
    match exn with
    | Unix_error (EADDRINUSE, _, _) ->
      unlink @@ socket_file paths |> Lwt.ignore_result;
      sock >>= bind_and_listen
    | _ ->
      Logs_lwt.err (fun m -> m "Failed to bind socket: %s" @@ Exn.to_string exn)
      >>= (fun () -> return @@ Error(K_error.FailedToCreateSocket)))

