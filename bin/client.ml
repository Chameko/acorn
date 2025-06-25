open Lwt
open Resources

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
let handle_connection ic oc msg =
  let%lwt () = Logs_lwt.debug (fun m -> m "Sending to server") in
  (* Get the message *)
  try%lwt
    let%lwt () = Lwt_io.write_line oc msg in
    let%lwt () = Logs_lwt.debug (fun m -> m "Reading server response") in
    let%lwt msg = Lwt_io.read_line_opt ic in
    return_ok msg
  with
  | Unix.Unix_error (e, _, _) ->
    return_error
    @@ K_error.SocketIOFailure
         ("Failed to communicate with the server: " ^ Unix.error_message e)
;;

let help_message () =
  Logs_lwt.app (fun m ->
    m "Input commands. Type ':q' to quit and 'help' to output this message.")
;;

(** Main client loop *)
let rec client_loop ic oc =
  let%lwt () = Logs_lwt.debug (fun m -> m "Reading line") in
  let%lwt msg = Lwt_io.(read_line stdin) in
  let%lwt msg = return @@ Stdlib.String.trim msg in
  (* Process server messsage *)
  match msg with
  | "help" ->
    let%lwt () = help_message () in
    return_ok ()
  | ":q" -> return_ok ()
  | msg ->
    (match%lwt handle_connection ic oc msg with
     | Ok (Some msg) ->
       let%lwt () = Logs_lwt.app (fun m -> m "%s" msg) in
       client_loop ic oc
     | Ok None -> client_loop ic oc
     | Error e -> return_error e)
;;

(** Perform a single request *)
let single_request ic oc req =
  let rec wait_for_response () =
    match%lwt handle_connection ic oc req with
    | Ok (Some msg) ->
      let%lwt () = Logs_lwt.app (fun m -> m "%s" msg) in
      wait_for_response ()
    | Ok None -> return_unit
    | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
  in
  wait_for_response ()
;;

(** Start the client *)
let start_client ?req paths =
  let%lwt () =
    Logs_lwt.debug (fun m ->
      m "Request: %a" (Format.pp_print_option Format.pp_print_string) req)
  in
  match%lwt connect paths () with
  | Ok sock ->
    (try%lwt
       let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
       let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
       match req with
       | Some req -> single_request ic oc req
       | None ->
         let%lwt () = help_message () in
         (match%lwt client_loop ic oc with
          | Ok _ -> return_unit
          | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e))
     with
     | Unix.Unix_error (e, _, _) ->
       Logs_lwt.err (fun m ->
         m "%s"
         @@ K_error.output
         @@ K_error.SocketIOFailure
              ("Failed to establish socket I/O channels: " ^ Unix.error_message e)))
  | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
;;
