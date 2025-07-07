(** A kakoune buffer *)
type buffer =
  { name : string (** Buffer name*)
  ; path : string (** Buffer FIFO path *)
  ; ic : Lwt_io.input_channel (** Buffer FIFO input channel *)
  }

(** A kakoune session *)
type session =
  { id : int (** Kakoune session *)
  ; buffers : buffer list ref (** Buffers for the session *)
  }

(** Execute a kakoune command *)
let kak_cmd_send session =
  Lwt_process.with_process_out ("kak", [| "kak"; "-p"; Int.to_string session.id |])
;;

let kak_connect paths session =
  let open Lwt_unix in
  (* Create the socket  *)
  let sock = Resources.create_socket () in
  try%lwt
    (* Connect to the socket *)
    match sock with
    | Ok sock ->
      let%lwt () = connect sock @@ ADDR_UNIX (Resources.kak_socket paths session) in
      Lwt.return_ok sock
    | Error e -> Lwt.return_error e
  with
  | Unix.Unix_error (e, _, _) ->
    Lwt.return_error
    @@ K_error.SocketIOFailure ("Failed to connect to socket: " ^ Unix.error_message e)
;;

(** Create a kakoune session *)
let kak_session session paths = { buffers = ref []; id = session }

let kak_sock_send paths session msg =
  let open Stdint in
  (* Connect to socket *)
  match%lwt kak_connect paths session.id with
  | Ok sock ->
    (* Create bytes of length magic byte + byte length (u32) + message length (u32)*)
    let header = Bytes.create (1 + 4 + 4) in
    (* Inform socket that its recieving commands*)
    Bytes.set_uint8 header 0 0x02;
    (* Add line feed to message *)
    let msg = String.cat msg (Char.chr 0x0a |> Base.String.of_char) in
    (* Add length of whole message AND length of just string *)
    if Sys.big_endian
    then (
      Uint32.to_bytes_big_endian (String.length msg + 4 + 4 + 1 |> Uint32.of_int) header 1;
      Uint32.to_bytes_big_endian (String.length msg |> Uint32.of_int) header (1 + 4))
    else (
      Uint32.to_bytes_little_endian
        (String.length msg + 4 + 4 + 1 |> Uint32.of_int)
        header
        1;
      Uint32.to_bytes_little_endian (String.length msg |> Uint32.of_int) header (1 + 4));
    (* Add a line feed *)
    let bytes = Bytes.cat header (String.to_bytes msg) in
    let%lwt sent = Lwt_unix.write sock bytes 0 (Bytes.length bytes) in
    if Int.equal sent (Bytes.length bytes)
    then Lwt.return_unit
    else (
      let%lwt () =
        Logs_lwt.debug (fun m -> m "Failed to send via socket. Fallback to kak -p")
      in
      kak_cmd_send session (fun oc -> Lwt_io.write oc#stdin msg))
  | Error e ->
    let%lwt () =
      Logs_lwt.debug (fun m -> m "%s. Fallback to kak -p" (K_error.output e))
    in
    kak_cmd_send session (fun oc -> Lwt_io.write oc#stdin msg)
;;
