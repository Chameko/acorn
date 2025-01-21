open Acorn
open Format

(** Used to generate CST *)
let gen_cst text =
  let tokens = Lexer.parse_text text in
  (Parser.parse (Array.of_list tokens)).ast

let test text =
  printf "@[<v>%a@]@." Ast.Green.pp_cst (gen_cst text);
