(** Contains the methods for parsing the tokens into a CST *)

(** Contains the parser state *)
type t =
  { tokens : Token.t array
  ; cursor : int
  }
