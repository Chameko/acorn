(** Contains the methods for parsing the tokens into a CST *)

type t = {
    tokens : Token.t array;
    cursor : int;
    ast : Ast.Green.t;
  }
(** Contains the parser state *)

val parse : Token.t array -> t
(** parse an array of tokens *)
