open Lwt
open Resources
open Paths

(** Connect to a socket *)
let connect paths () =
  let open Lwt_unix in
  (* Create the socket  *)
  let sock = create_socket () in
  try%lwt
    (* Connect to the socket *)
    match sock with
    | Ok sock ->
      let%lwt () = connect sock @@ ADDR_UNIX (socket_file paths) in
      return_ok sock
    | Error e -> return_error e
  with
  | Unix.Unix_error (e, _, _) ->
    return_error
    @@ K_error.SocketIOFailure ("Failed to connect to socket: " ^ Unix.error_message e)
;;

(** Handles the conenction *)
let handle_connection ic oc msg () =
  let%lwt () = Logs_lwt.debug (fun m -> m "Sending to server") in
  (* Get the message *)
  let msg =
    try%lwt
      let%lwt () = Lwt_io.write_line oc msg in
      let%lwt () = Logs_lwt.debug (fun m -> m "Reading server response") in
      return_ok @@ Lwt_io.read_line_opt ic
    with
    | Unix.Unix_error (e, _, _) ->
      return_error
      @@ K_error.SocketIOFailure
           ("Failed to communicate with the server: " ^ Unix.error_message e)
  in
  (* Process server response *)
  match%lwt msg with
  | Ok msg ->
    (match%lwt msg with
     | Some res ->
       let%lwt () = Logs_lwt.app (fun m -> m "%s" res) in
       return_ok true
     | None ->
       let%lwt () = Logs_lwt.debug (fun m -> m "Connection closed") in
       return_ok false)
  | Error e -> return_error e
;;

(** Main client loop *)
let rec client_loop ic oc () =
  let%lwt () = Logs_lwt.debug (fun m -> m "Reading line") in
  let%lwt msg = Lwt_io.(read_line stdin) in
  let%lwt msg = return @@ Stdlib.String.trim msg in
  (* Process server messsage *)
  match msg with
  | "exit" -> return_ok ()
  | msg ->
    (match%lwt handle_connection ic oc msg () with
     | Ok cont -> if cont then client_loop ic oc () else return_ok ()
     | Error e -> return_error e)
;;

(** Start the client *)
let start_client paths =
  match%lwt connect paths () with
  | Ok sock ->
    (try%lwt
       let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
       let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
       match%lwt client_loop ic oc () with
       | Ok () -> return_unit
       | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
     with
     | Unix.Unix_error (e, _, _) ->
       Logs_lwt.err (fun m ->
         m "%s"
         @@ K_error.output
         @@ K_error.SocketIOFailure
              ("Failed to establish socket I/O channels: " ^ Unix.error_message e)))
  | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
;;
