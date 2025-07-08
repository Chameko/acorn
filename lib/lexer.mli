(** The lexer is responsible for converting a string of text into tokens. *)

(** Contains the lexer state *)
type t

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool

(** Lex some text *)
val lex_text : string -> Token.t Dynarray.t
