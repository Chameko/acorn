type session =
  { id : int (** Kakoune session *)
  ; buffers : string list (** Buffers for the session *)
  }

(** Create a kakoune session *)
let kak_session session = Lwt.return_ok { buffers = []; id = session }

(** Execute a kakoune command *)
let kak_oc session =
  Lwt_process.with_process_out ("kak", [| "kak"; "-p"; Int.to_string session.id |])
;;
