open Core
open Lwt
open Resources
open Paths

(** Connect to a socket *)
let connect paths () =
  let open Lwt_unix in
  let sock = create_socket () in
  Lwt_result.bind_lwt sock (fun sock -> connect sock @@ ADDR_UNIX(socket_file paths)
  >>= (fun () -> return sock))

let handle_connection ic oc msg () =
  Logs_lwt.debug (fun m -> m "Sending to server")
  >>= (fun () -> Lwt_io.write_line oc msg)
  >>= (fun () -> Logs_lwt.debug (fun m -> m "Reading server response"))
  >>= fun () -> (Lwt_io.read_line_opt ic >>=
    (fun msg ->
    match msg with
    | Some res ->
      Logs_lwt.app (fun m -> m "%s" res) >>= fun () -> return true
    | None ->
      Logs_lwt.info (fun m -> m "Connection closed") >>= fun () -> return false))

let rec client_loop ic oc () =
  let process_msg l =
    match l with
    | "exit" -> Lwt.return_unit
    | l -> handle_connection ic oc l () >>= (fun cont -> if cont then client_loop ic oc () else return ())
  in
  Logs_lwt.debug (fun m -> m "Reading line")
  >>= (fun () -> Lwt_io.(read_line stdin))
  >>= (fun l -> Logs_lwt.debug (fun m -> m "Handling line") >>= (fun () -> return l))
  >|= (fun l -> Stdlib.String.trim l)
  >>= process_msg

let start_client paths =
  catch (connect paths) (fun exn ->
  Logs_lwt.err (fun m -> m "Failed to connect to socket: %s" @@ Exn.to_string exn)
  >>= (fun () -> return_error K_error.FailedConnection))
  >>= fun sfd ->
    match sfd with
    | Ok sfd ->
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sfd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sfd in
      client_loop ic oc () >>= (fun () -> Lwt_unix.close sfd)
    | Error e -> Logs_lwt.err (fun m -> m "Failed to create socket: %s" (K_error.output e))
