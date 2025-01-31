open Core

(** Check if the server is running already *)
let is_server_running paths =
  let pid_file = Paths.pid_file paths in
  try
    (* Read contents of PDI file *)
    let ic = In_channel.create ?binary:(Some false) pid_file in
    let contents = In_channel.input_all ic in
    (* Check which process the PID belongs to *)
    let comm =
      Core_unix.open_process_in
        ("ps -p "
         ^ Stdlib.String.trim contents
         ^ " -o comm=")
    in
    let out = In_channel.input_all comm in
    let _ = Core_unix.close_process_in comm in
    String.(out = "kakorn")
  with
  | Sys_error e ->
     let () =
       Printf.printf "Failed to identify the server is working: %s" e
     in
     false
  | Core_unix.Unix_error (e, _, _) ->
     let () =
       Printf.printf "Failed to identify the server is working: %s"
       (Core_unix.Error.message e)
     in
     false


let start_server paths =
  (* Check if the server is running *)
  if is_server_running paths then
    Error K_error.ServerAlreadyRunning
  else
    (* Create PID file and dir *)
    Paths.create_files paths

