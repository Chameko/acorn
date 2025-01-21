type t = {
    source: string;
    position: int;
  }
[@@deriving show, eq]

let parse_text src =
  (* Create the lexer *)
  let lex = { source = src; position = 0; } in
  let get_char lex =
    if lex.position < String.length lex.source then
      Some lex.source.[lex.position]
    else
      None
  in

  (* Gets the next token in the lexer *)
  let next_token lex =
    get_char lex, { source = lex.source; position = lex.position + 1} in 

  (* Lexes a token *)
  let lex_token lex =
    let (c, lex) = next_token lex in
    match c with
    | Some c -> (
      match c with
      | '-' -> Some Token.Dash
      | '*' -> Some Token.Star
      | '/' -> Some Token.Slash
      | '_' -> Some Token.Underscore
      | '`' -> Some Token.Backtick
      | '~' -> Some Token.Tilde
      | '^' -> Some Token.Carret
      | '[' -> Some Token.LSquareB
      | ']' -> Some Token.RSquareB
      | '(' -> Some Token.LParen
      | ')' -> Some Token.RParen
      | '.' -> Some Token.Dot
      | '=' -> Some Token.Equals
      | ',' -> Some Token.Comma
      | '\n' -> Some Token.Newline
      | '@' -> (
        match (next_token lex) with
        | (Some '@', _) -> Some Token.AtAt
        | (Some '+', _) -> Some Token.AtPlus
        | (Some '=', _) -> Some Token.AtEquals
        | _ -> Some (Token.Text "@")
      )
      | ' ' -> Some (Token.Whitespace " ")
      | c -> Some (Token.Text (String.make 1 c))), lex
    | None -> None, lex
  in

  (* Recursivly lexes the text *)
  let rec lex_tokens lex tokens =
    match lex_token lex with
    (* If we find previous text, we concat this to it *)
    | (Some Token.Text t, lex) -> (
       match try Some (List.hd tokens) with _ -> None with
       | Some Token.Text t2 ->
          lex_tokens lex ((Token.Text (t2 ^ t) :: List.tl tokens))
       | _ -> lex_tokens lex ((Token.Text t) :: tokens)
    )
    (* Same for whitespace *)
    | (Some Token.Whitespace w, lex) -> (
       match try Some(List.hd tokens) with _ -> None with
       | Some Token.Whitespace w2 ->
          lex_tokens lex ((Token.Whitespace (w2 ^ w) :: List.tl tokens))
       | _ -> lex_tokens lex ((Token.Whitespace w) :: tokens)
    )
    | (Some tk, lex) -> lex_tokens lex (tk :: tokens)
    | _ -> tokens
  in

  (* We flip our tokens so they're in the right order*)
  lex_tokens lex [] |> List.rev
