(** The parser state *)
type t

(** Parse some text *)
val parse : string -> Ast.contents list
