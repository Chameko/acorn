open Unix
open Paths
open Lwt
open Stdio
open Base

(** Create the pid file *)
let create_pid paths =
  let open Result in

  Logs.debug (fun m -> m "Creating PID file");
  let pid_file = pid_file paths in

  (* Create out channel to file (creates file if needed )*)
  let oc =
    try
      Ok (Out_channel.create pid_file ?append:(Some false))
    with
    | Sys_error e ->
      Error (K_error.FileIOFailure ("Failed to create PID file: " ^ e))
  in

  (* Write the PID *)
  let write_pid oc =
    Exn.protect ~f:(fun () ->
      try
        (* Read the file *)
        Ok (Out_channel.output_string oc (Int.to_string @@ Unix.getpid ()))
      with Sys_error e ->
        Error (K_error.FileIOFailure ("Failed to write to PID file: " ^ e)))
    (* Close the out channel *)
    ~finally:(fun () -> Out_channel.close oc)
  in
  oc >>= write_pid

(** Create the directory *)
let create_dir paths =
  Logs.debug (fun m -> m "Creating kakorn directory");
  try
    (* Create the directory if needed *)
    Ok(Unix.mkdir paths.runtime_dir 0o755)
  with
  (* If the directory already exits thats fine *)
  | Unix_error (EEXIST, _, _) ->
    Logs.debug (fun m -> m "Using existing runtime directory");
    Ok (())
  (* Report any other error *)
  | Unix_error (e, _, _) ->
    Error (K_error.FileIOFailure ("Failed to create runtime directory: " ^ error_message e))

(** Create socket *)
let create_socket () =
  let open Lwt_unix in
  try
    Ok(socket PF_UNIX SOCK_STREAM 0)
  with
  | Unix_error (e, _, _) ->
    Error(K_error.SocketCreationFailure ("Failed to create socket: " ^ error_message e))

(** Create a socket for the server *)
let create_server_socket paths () =
  let open Lwt_unix in
  
  let bind_and_listen () =
    let sock = create_socket() in
    match sock with
    | Ok sock ->
      let%lwt () = bind sock @@ ADDR_UNIX (socket_file paths) in
      listen sock 10;
      return_ok sock
    | Error e -> return_error e
  in

  try%lwt
    bind_and_listen ()
  with
  | Unix_error (EADDRINUSE, _, _) ->
    let%lwt () = Logs_lwt.debug (fun m -> m "Socket already exists. Deleting and trying again.") in
    (try%lwt
      let%lwt () = Lwt_unix.unlink @@ socket_file paths in
      bind_and_listen ()
    with
    | Unix_error (e, _, _) ->
      return_error @@ K_error.SocketCreationFailure ("Failed to bind to socket: " ^ error_message e))
  | Unix_error (e, _, _) ->
    return_error @@ K_error.SocketCreationFailure ("Failed to bind socket: " ^ error_message e)

