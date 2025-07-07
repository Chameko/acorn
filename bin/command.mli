(** Commands to the client *)
module Client : sig
  (** Messages to send to the client *)
  type reply =
    | Debug of int (** Debug messages *)
    | Output of int (** Messages to write to stdout *)
    | End (** End of messages *)

  (** Convert a reply to sexp *)
  val reply_of_sexp : Sexplib0.Sexp.t -> reply

  (** Convert a sexp to reply *)
  val sexp_of_reply : reply -> Sexplib0.Sexp.t

  (** Send a debug message to the client *)
  val client_debug : string -> Lwt_io.output_channel -> unit Lwt.t

  (** Send an message for the client to output*)
  val client_output : string -> Lwt_io.output_channel -> unit Lwt.t
end

(** Commands to the server *)
module Server : sig
  (** Messages to send to the server *)
  type command =
    | Term (** Terminate the server *)
    | OpenBuffer of
        { session : int (** The kak session *)
        ; name : string (** The name of the buffer *)
        } (** Open a kak buffer *)
    | CloseBuffer of
        { session : int (** The kak session *)
        ; name : string (** The name of the buffer *)
        } (** Close a kak buffer *)
    | Init of int (** Initialise a kak session *)

  (** Convert a command to sexp *)
  val command_of_sexp : Sexplib0.Sexp.t -> command

  (** Convert a sexp to a command *)
  val sexp_of_command : command -> Sexplib0.Sexp.t
end
