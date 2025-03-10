open Base

type paths = { runtime_dir : string }

(** Get the important paths/files *)
let paths () =
  (* Get the runtime dir*)
  match Sys.getenv "XDG_RUNTIME_DIR" with
  | Some d -> Ok { runtime_dir = Stdlib.Filename.concat d "kakorn" }
  | None -> (
      match Sys.getenv "TMPDIR" with
      | Some d -> Ok { runtime_dir = Stdlib.Filename.concat d "kakorn" }
      | None -> Error (K_error.FileIOFailure "No runtime directory"))

let pid_file paths = Stdlib.Filename.concat paths.runtime_dir "pid"

let buff_files paths name =
  Stdlib.Filename.concat paths.runtime_dir
  @@ Stdlib.Filename.concat "buffs" name

let stderr_file paths = Stdlib.Filename.concat paths.runtime_dir "stderr.txt"
let stdout_file paths = Stdlib.Filename.concat paths.runtime_dir "stdout.txt"
let socket_file paths = Stdlib.Filename.concat paths.runtime_dir "socket"
