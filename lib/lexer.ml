open Token

let get_char lex source = if lex < String.length source then Some source.[lex] else None

(* Gets the next char in the lexer *)
let next_char lex source =
  let char = get_char (lex + 1) source in
  char, lex + 1
;;

(* Lexes a token *)
let rec get_token lex source =
  let c, lex = next_char lex source in
  match c with
  | None -> None, lex
  | Some c ->
    (match c with
     | '-' -> Some { ty = Token.Dash; location = { offset = lex; length = 1 } }, lex
     | '*' -> Some { ty = Token.Star; location = { offset = lex; length = 1 } }, lex
     | '/' -> Some { ty = Token.Slash; location = { offset = lex; length = 1 } }, lex
     | '_' -> Some { ty = Token.Underscore; location = { offset = lex; length = 1 } }, lex
     | '`' -> Some { ty = Token.Backtick; location = { offset = lex; length = 1 } }, lex
     | '~' -> Some { ty = Token.Tilde; location = { offset = lex; length = 1 } }, lex
     | '^' -> Some { ty = Token.Carret; location = { offset = lex; length = 1 } }, lex
     | '[' -> Some { ty = Token.LSquareB; location = { offset = lex; length = 1 } }, lex
     | ']' -> Some { ty = Token.RSquareB; location = { offset = lex; length = 1 } }, lex
     | '(' -> Some { ty = Token.LParen; location = { offset = lex; length = 1 } }, lex
     | ')' -> Some { ty = Token.RParen; location = { offset = lex; length = 1 } }, lex
     | '.' -> Some { ty = Token.Dot; location = { offset = lex; length = 1 } }, lex
     | '=' -> Some { ty = Token.Equals; location = { offset = lex; length = 1 } }, lex
     | ',' -> Some { ty = Token.Comma; location = { offset = lex; length = 1 } }, lex
     | '\n' -> Some { ty = Token.Newline; location = { offset = lex; length = 1 } }, lex
     | '@' ->
       (match next_char lex source with
        | Some '@', _ ->
          Some { ty = Token.AtAt; location = { offset = lex; length = 2 } }, lex
        | Some '+', _ ->
          Some { ty = Token.AtPlus; location = { offset = lex; length = 2 } }, lex
        | Some '=', _ ->
          Some { ty = Token.AtEquals; location = { offset = lex; length = 2 } }, lex
        | _ -> Some { ty = Token.Text "@"; location = { offset = lex; length = 1 } }, lex)
     | ' ' ->
       (match get_token lex source with
        | Some { ty = Token.Whitespace w; location = location2 }, lex2 ->
          ( Some
              { ty = Token.Whitespace (w ^ " ")
              ; location = { offset = lex; length = location2.length + 1 }
              }
          , lex2 )
        | _ ->
          Some { ty = Token.Whitespace " "; location = { offset = lex; length = 1 } }, lex)
     | c ->
       (match get_token lex source with
        | Some { ty = Token.Text t; location = location2 }, lex2 ->
          ( Some
              { ty = Token.Text (String.make 1 c ^ t)
              ; location = { offset = lex; length = location2.length + 1 }
              }
          , lex2 )
        | _ ->
          ( Some
              { ty = Token.Text (String.make 1 c)
              ; location = { offset = lex; length = 1 }
              }
          , lex )))
;;

let lex_text src =
  let rec lex_tokens lex tokens =
    match get_token lex src with
    | Some tk, lex ->
      Dynarray.add_last tokens tk;
      lex_tokens lex tokens
    | _ -> tokens
  in
  (* We start at -1 as we always march forward 1 *)
  let lex = -1 in
  let tokens = Dynarray.create () in
  lex_tokens lex tokens
;;
