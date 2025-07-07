(** A buffer from kakoune *)
type buffer =
  { name : string
  ; path : string
  ; ic : Lwt_io.input_channel
  }

(** A kakoune session *)
type session =
  { id : int
  ; buffers : buffer list ref
  }

(** Send a command to kakoune via the shell *)
val kak_cmd_send : session -> (Lwt_process.process_out -> 'a Lwt.t) -> 'a Lwt.t

(** Connect to kakoune's socket *)
val kak_connect
  :  Resources.paths
  -> int
  -> (Lwt_unix.file_descr, K_error.error) result Lwt.t

(** Create a kakoune session*)
val kak_session : int -> 'a -> session

(** Send a command via kakoune's socket *)
val kak_sock_send : Resources.paths -> session -> string -> unit Lwt.t
