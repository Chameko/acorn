open Sexplib.Std

(** Commands to send to the server *)
type command =
  | Term (** Terminate the server *)
  | OpenBuffer of
      { session : int
      ; name : string
      } (** Report the buffer name *)
  | CloseBuffer of
      { session : int
      ; name : string
      }
  | Init of int (** Initialise a kak session *)
[@@deriving sexp]
