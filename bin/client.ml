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

let rec handle_message ic =
  let open Sexplib in
  let open Command in
  match%lwt Lwt_io.read_line_opt ic with
  | None -> Logs_lwt.debug (fun m -> m "Terminating...")
  | Some msg ->
    (match Sexp.of_string_conv msg Client.reply_of_sexp with
     | `Result (Client.Debug len) ->
       let%lwt msg = Lwt_io.read ~count:len ic in
       let%lwt () = Logs_lwt.debug (fun m -> m "%s" msg) in
       handle_message ic
     | `Result (Client.Output len) ->
       let%lwt msg = Lwt_io.read ~count:len ic in
       let%lwt () = Logs_lwt.app (fun m -> m "%s" msg) in
       handle_message ic
     | `Result Client.End -> Logs_lwt.debug (fun m -> m "End of messages")
     | `Error (exn, _) ->
       Logs_lwt.debug (fun m -> m "Bad request: %s" (Base.Exn.to_string exn)))
;;

(** Handles the conenction *)
let handle_connection ic oc req =
  let%lwt () = Logs_lwt.debug (fun m -> m "Sending to server") in
  (* Get the message *)
  try%lwt
    let%lwt () = Lwt_io.write_line oc req in
    let%lwt () = Logs_lwt.debug (fun m -> m "Reading server response") in
    handle_message ic
  with
  | Unix.Unix_error (e, _, _) ->
    Logs_lwt.debug (fun m ->
      m "%s"
      @@ K_error.output
           (K_error.SocketIOFailure
              ("Failed to communicate with the server: " ^ Unix.error_message e)))
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
    return_unit
  | ":q" -> return_unit
  | msg ->
    let%lwt () = handle_connection ic oc msg in
    let%lwt () = Logs_lwt.app (fun m -> m "-------------------------------------") in
    client_loop ic oc
;;

(** Start the client *)
let start_client ?req paths =
  let%lwt () =
    Logs_lwt.debug (fun m ->
      m "Request: %a" (Format.pp_print_option Format.pp_print_string) req)
  in
  match%lwt connect paths () with
  | Ok sock ->
    Lwt.finalize
      (fun () ->
         try%lwt
           let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
           let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
           match req with
           | Some req -> handle_connection ic oc req
           | None ->
             let%lwt () = help_message () in
             client_loop ic oc
         with
         | Unix.Unix_error (e, _, _) ->
           Logs_lwt.err (fun m ->
             m "%s"
             @@ K_error.output
             @@ K_error.SocketIOFailure
                  ("Failed to establish socket I/O channels: " ^ Unix.error_message e)))
      (fun () -> Lwt_unix.close sock)
  | Error e -> Logs_lwt.err (fun m -> m "%s" @@ K_error.output e)
;;
