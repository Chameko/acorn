(** The lexer is responsible for converting a string of text into tokens. *)

(** Lex some text *)
val lex_text : string -> Token.t Dynarray.t
