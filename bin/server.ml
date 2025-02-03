open Core
open Lwt
open Resources

let counter = ref 0

(** Message handling *)
let handle_message msg =
  match msg with
  | "read" -> string_of_int !counter
  | "inc" -> counter := !counter + 1; "Counter has been incramented"
  | _ -> "Unknown Command"

(** Handle the connection *)
let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
    match msg with
    | Some msg ->
      let reply = handle_message msg in
      Lwt_io.write_line oc reply >>= handle_connection ic oc
    | None -> 
      Logs_lwt.info (fun m -> m "Connection closed") >>= return)

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


(** Accept a connection *)
let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Exn.to_string e)));
  Logs_lwt.info (fun m -> m "New connection") >>= return

let rec serve sock () =
  Lwt_unix.accept sock >>= accept_connection >>= serve sock

(** Start server *)
let start_server paths =
  (* Check if the server is running *)
  if is_server_running paths then
    Logs_lwt.err (fun m -> m "%s" @@ K_error.output K_error.ServerAlreadyRunning) >>= return
  else
    let res = Result.(
      (* Create PID file and dir *)
      create_files paths
      (* Create socket *)
      >>| create_server_socket paths
      (* Serve the socket *)
      >>| fun s -> Lwt.bind s (fun s -> serve s ()))
    in
    match res with
    | Ok serve -> serve
    | Error e ->
      Logs_lwt.err (fun m -> m "%s" @@ K_error.output e) >>= return
