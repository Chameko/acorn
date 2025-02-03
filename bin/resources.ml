open Paths
open Lwt
open Core
open Core_unix

(** Create the pid file *)
let create_pid paths =
  let open Result in
  let pid_file = pid_file paths in
  (* Create out channel to file (creates file if needed )*)
  let oc =
    try Ok (Out_channel.create pid_file ?append:(Some false))
    with Sys_error e -> Error (K_error.IOError (pid_file, e))
  in
  (* Write the PID *)
  let write_pid oc =
    try
      Ok (Out_channel.output_string oc (Pid.to_string @@ getpid ());
          Out_channel.close oc)
    with Sys_error e -> Error (K_error.IOError (pid_file, e))
  in
  oc >>= write_pid

(** Create the neccessary files and the dir *)
let create_files paths =
  let dir = try
    (* Create the directory if needed *)
    Ok(Core_unix.mkdir paths.runtime_dir)
    with
    (* If the directory already exits thats fine *)
    | Unix_error (Error.EEXIST, _, _) -> Ok (())
    (* Report any other error *)
    | Unix_error (e, _, dir) -> Error (K_error.IOError (dir, (Error.message e)))
  in
  match dir with
  | Ok _ ->
     (* Create the important files *)
     create_pid paths
  | Error e -> Error e

(** Create socket *)
let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_UNIX SOCK_STREAM 0 in
  sock

(** Create a socket for the server *)
let create_server_socket paths () =
  let open Lwt_unix in
  let sock = create_socket () in
  bind sock @@ ADDR_UNIX(socket_file paths) >>=
    fun () -> listen sock 10; return sock
