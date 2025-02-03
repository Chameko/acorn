open Core
open Lwt
open Resources
open Paths

(** Connect to a socket *)
let connect paths () =
  let open Lwt_unix in
  let sock = create_socket () in
  connect sock @@ ADDR_UNIX(socket_file paths) >>= fun () -> return @@ Ok(sock)

let handle_connection ic oc msg () =
  Lwt_io.write oc msg >>=
    fun () -> (Lwt_io.read_line_opt ic >>=
      (fun msg ->
      match msg with
      | Some res ->
        Logs_lwt.app (fun m -> m "%s" res) >>= return
      | None ->
        Logs_lwt.info (fun m -> m "Connection closed") >>= return))

let rec client_loop ic oc () =
  let process_msg l =
    match l with
    | "exit" -> Lwt.return_unit
    | l -> handle_connection ic oc l () >>= client_loop ic oc
  in
  Lwt_io.(read_line stdin) >|= (fun l -> Stdlib.String.trim l)
  >>= process_msg

let start_client paths =
  catch (connect paths) (fun exn ->
  return (Error (K_error.FailedConnection (Exn.to_string exn)))) >>= fun sfd ->
    match sfd with
    | Ok sfd ->
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sfd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sfd in
      client_loop ic oc ()
    | Error e -> Logs_lwt.err (fun m -> m "%s" (K_error.output e))
