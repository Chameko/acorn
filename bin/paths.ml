open Core
open Core_unix

type paths = {
    runtime_dir : string;
  }

(** Get the important paths/files *)
let paths () =
  (* Get the runtime dir*)
  match Sys.getenv ("XDG_RUNTIME_DIR") with
  | Some d -> Ok { runtime_dir = Filename.concat d "kakorn"}
  | None ->
     match Sys.getenv ("TMPDIR") with
     | Some d -> Ok { runtime_dir = Filename.concat d "kakorn"}
     | None -> Error K_error.NoRuntimeDir

let pid_file paths =
  Filename.concat paths.runtime_dir "pid"

let buff_files paths name =
  Filename.concat paths.runtime_dir @@ Filename.concat "buffs" name

let stderr_file paths =
  Filename.concat paths.runtime_dir "stderr.txt"

let stdout_file paths =
  Filename.concat paths.runtime_dir "stdout.txt"

let socket_file paths =
  Filename.concat paths.runtime_dir "socket"

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
