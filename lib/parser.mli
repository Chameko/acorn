(** Contains the methods for parsing the tokens into a CST *)

type t = { tokens : Token.t array; cursor : int }
(** Contains the parser state *)
