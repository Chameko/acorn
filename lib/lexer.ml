open Token

type t =
  { source : string
  ; position : int
  ; line : int
  }
[@@deriving show, eq]

let parse_text src =
  (* Create the lexer *)
  let lex = { source = src; position = 0; line = 1 } in
  let get_char lex =
    if lex.position < String.length lex.source
    then Some lex.source.[lex.position]
    else None
  in
  (* Gets the next char in the lexer *)
  let next_char lex = get_char lex, { lex with position = lex.position + 1 } in
  (* Lexes a token *)
  let rec lex_token lex =
    let c, lex = next_char lex in
    match c with
    | None -> None, lex
    | Some c ->
      (match c with
       | '-' ->
         ( Some
             { ty = Token.Dash
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '*' ->
         ( Some
             { ty = Token.Star
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '/' ->
         ( Some
             { ty = Token.Slash
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '_' ->
         ( Some
             { ty = Token.Underscore
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '`' ->
         ( Some
             { ty = Token.Backtick
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '~' ->
         ( Some
             { ty = Token.Tilde
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '^' ->
         ( Some
             { ty = Token.Carret
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '[' ->
         ( Some
             { ty = Token.LSquareB
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | ']' ->
         ( Some
             { ty = Token.RSquareB
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '(' ->
         ( Some
             { ty = Token.LParen
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | ')' ->
         ( Some
             { ty = Token.RParen
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '.' ->
         ( Some
             { ty = Token.Dot
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '=' ->
         ( Some
             { ty = Token.Equals
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | ',' ->
         ( Some
             { ty = Token.Comma
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , lex )
       | '\n' ->
         ( Some
             { ty = Token.Newline
             ; location = { start = lex.position; stop = lex.position; line = lex.line }
             }
         , { lex with line = lex.line + 1; position = 1 } )
       | '@' ->
         (match next_char lex with
          | Some '@', _ ->
            ( Some
                { ty = Token.AtAt
                ; location =
                    { start = lex.position; stop = lex.position; line = lex.line }
                }
            , lex )
          | Some '+', _ ->
            ( Some
                { ty = Token.AtPlus
                ; location =
                    { start = lex.position - 1; stop = lex.position; line = lex.line }
                }
            , lex )
          | Some '=', _ ->
            ( Some
                { ty = Token.AtEquals
                ; location =
                    { start = lex.position - 1; stop = lex.position; line = lex.line }
                }
            , lex )
          | _ ->
            ( Some
                { ty = Token.Text "@"
                ; location =
                    { start = lex.position - 1; stop = lex.position - 1; line = lex.line }
                }
            , lex ))
       | ' ' ->
         (match lex_token lex with
          | Some { ty = Token.Whitespace w; location = location2 }, lex2 ->
            ( Some
                { ty = Token.Whitespace (" " ^ w)
                ; location =
                    { start = lex.position; stop = location2.stop; line = lex.line }
                }
            , lex2 )
          | _ ->
            ( Some
                { ty = Token.Whitespace " "
                ; location =
                    { start = lex.position; stop = lex.position; line = lex.line }
                }
            , lex ))
       | c ->
         (match lex_token lex with
          | Some { ty = Token.Text t; location = location2 }, lex2 ->
            ( Some
                { ty = Token.Text (String.make 1 c ^ t)
                ; location =
                    { start = lex.position; stop = location2.stop; line = lex.line }
                }
            , lex2 )
          | _ ->
            ( Some
                { ty = Token.Text (String.make 1 c)
                ; location =
                    { start = lex.position; stop = lex.position; line = lex.line }
                }
            , lex )))
  in
  (* Recursivly lexes the text *)
  let rec lex_tokens lex tokens =
    match lex_token lex with
    | Some tk, lex -> lex_tokens lex (tk :: tokens)
    | _ -> tokens
  in
  (* We flip our tokens so they're in the right order*)
  lex_tokens lex [] |> List.rev
;;
