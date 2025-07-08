open Token

type t =
  { source : string
  ; mutable offset : int
  }
[@@deriving show, eq]

let get_char lex =
  if lex.offset < String.length lex.source then Some lex.source.[lex.offset] else None
;;

(* Gets the next char in the lexer *)
let next_char lex =
  let char = get_char lex in
  lex.offset <- lex.offset + 1;
  char, lex
;;

(* Lexes a token *)
let rec get_token lex =
  let c, lex = next_char lex in
  match c with
  | None -> None, lex
  | Some c ->
    (match c with
     | '-' ->
       Some { ty = Token.Dash; location = { offset = lex.offset; length = 1 } }, lex
     | '*' ->
       Some { ty = Token.Star; location = { offset = lex.offset; length = 1 } }, lex
     | '/' ->
       Some { ty = Token.Slash; location = { offset = lex.offset; length = 1 } }, lex
     | '_' ->
       Some { ty = Token.Underscore; location = { offset = lex.offset; length = 1 } }, lex
     | '`' ->
       Some { ty = Token.Backtick; location = { offset = lex.offset; length = 1 } }, lex
     | '~' ->
       Some { ty = Token.Tilde; location = { offset = lex.offset; length = 1 } }, lex
     | '^' ->
       Some { ty = Token.Carret; location = { offset = lex.offset; length = 1 } }, lex
     | '[' ->
       Some { ty = Token.LSquareB; location = { offset = lex.offset; length = 1 } }, lex
     | ']' ->
       Some { ty = Token.RSquareB; location = { offset = lex.offset; length = 1 } }, lex
     | '(' ->
       Some { ty = Token.LParen; location = { offset = lex.offset; length = 1 } }, lex
     | ')' ->
       Some { ty = Token.RParen; location = { offset = lex.offset; length = 1 } }, lex
     | '.' -> Some { ty = Token.Dot; location = { offset = lex.offset; length = 1 } }, lex
     | '=' ->
       Some { ty = Token.Equals; location = { offset = lex.offset; length = 1 } }, lex
     | ',' ->
       Some { ty = Token.Comma; location = { offset = lex.offset; length = 1 } }, lex
     | '\n' ->
       Some { ty = Token.Newline; location = { offset = lex.offset; length = 1 } }, lex
     | '@' ->
       (match next_char lex with
        | Some '@', _ ->
          Some { ty = Token.AtAt; location = { offset = lex.offset; length = 2 } }, lex
        | Some '+', _ ->
          Some { ty = Token.AtPlus; location = { offset = lex.offset; length = 2 } }, lex
        | Some '=', _ ->
          ( Some { ty = Token.AtEquals; location = { offset = lex.offset; length = 2 } }
          , lex )
        | _ ->
          ( Some { ty = Token.Text "@"; location = { offset = lex.offset; length = 1 } }
          , lex ))
     | ' ' ->
       (match get_token lex with
        | Some { ty = Token.Whitespace w; location = location2 }, lex2 ->
          ( Some
              { ty = Token.Whitespace (w ^ " ")
              ; location = { offset = location2.offset; length = location2.length + 1 }
              }
          , lex2 )
        | _ ->
          ( Some
              { ty = Token.Whitespace " "
              ; location = { offset = lex.offset; length = 1 }
              }
          , lex ))
     | c ->
       (match get_token lex with
        | Some { ty = Token.Text t; location = location2 }, lex2 ->
          ( Some
              { ty = Token.Text (String.make 1 c ^ t)
              ; location = { offset = location2.offset; length = location2.length + 1 }
              }
          , lex2 )
        | _ ->
          ( Some
              { ty = Token.Text (String.make 1 c)
              ; location = { offset = lex.offset; length = 1 }
              }
          , lex )))
;;

let lex_text src =
  let rec lex_tokens lex tokens =
    match get_token lex with
    | Some tk, lex ->
      Dynarray.add_last tokens tk;
      lex_tokens lex tokens
    | _ -> tokens
  in
  let lex = { source = src; offset = 0 } in
  let tokens = Dynarray.create () in
  lex_tokens lex tokens
;;
