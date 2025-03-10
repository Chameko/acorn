(** The lexer is responsible for converting a string of text into tokens. *)

type t
(** Contains the lexer state *)

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val parse_text : string -> Token.t list
