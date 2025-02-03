open Core

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

