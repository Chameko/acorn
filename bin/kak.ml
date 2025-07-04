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

(** Create a kakoune session *)
let kak_session session = { buffers = ref []; id = session }

(** Execute a kakoune command *)
let kak_oc session =
  Lwt_process.with_process_out ("kak", [| "kak"; "-p"; Int.to_string session.id |])
;;

let inject = "\n"
