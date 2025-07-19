(** The parser state *)
type t =
  { tokens : Token.t array
  ; cursor : int
  }

(** Get the current token *)
let current p_state =
  if p_state.cursor < Array.length p_state.tokens
  then Some p_state.tokens.(p_state.cursor)
  else None
;;

(** Advance the p_state *)
let advance p_state = { p_state with cursor = p_state.cursor + 1 }

let init tokens = { tokens; cursor = 0 }

let check_number string =
  match int_of_string_opt string with
  | Some _ -> true
  | None -> false
;;

(******************************************************************************)
(* Combinators *)
(******************************************************************************)

let alternate p1 p2 p_state =
  match p1 p_state with
  | None -> p2 p_state
  | res -> res
;;

let sequence p1 p2 p_state =
  match p1 p_state with
  | None -> None
  | Some (res, p_state) ->
    (match p2 p_state with
     | None -> None
     | Some (res2, p_state) -> Some ((res, res2), p_state))
;;

let rec repeat_limit p1 limit p_state =
  match limit p_state with
  | Some (l, p_state) -> ([], Some l), p_state
  | None ->
    (match p1 p_state with
     | Some (res, p_state) ->
       let (tl, l), p_state = repeat_limit p1 limit p_state in
       (res :: tl, l), p_state
     | None -> ([], None), p_state)
;;

let rec repeat p1 p_state =
  match p1 p_state with
  | Some (res, p_state) ->
    let tl, p_state = repeat p1 p_state in
    res :: tl, p_state
  | None -> [], p_state
;;

let inverse p1 p_state =
  match p1 p_state with
  | Some _ -> None
  | None -> Some ((), p_state)
;;

(* Convenience operators *)
let ( |? ) p1 p2 = alternate p1 p2
let ( >! ) p1 limit = repeat_limit p1 limit
let ( >> ) p1 p2 = sequence p1 p2

let ( let* ) p1 construct =
  match p1 with
  | Some res -> Some (construct res)
  | None -> None
;;

(******************************************************************************)
(* Utilities *)
(******************************************************************************)

let beginning_check p_state = if p_state.cursor == 0 then Some ((), p_state) else None

let for_all_fmt_gen p1 =
  p1 Ast.Bold
  |? p1 Ast.Italics
  |? p1 Ast.Monospace
  |? p1 Ast.StrikeThrough
  |? p1 Ast.Underline
;;

let to_text p1 p_state =
  match p1 p_state with
  | Some (range, p_state) -> Some (Ast.Text range, p_state)
  | None -> None
;;

(******************************************************************************)
(* Parsers *)
(******************************************************************************)

(** Generate parsers that parse a single significant character *)
let simple_parse_gen ty =
  fun p_state ->
  match current p_state with
  | Some tk when tk.ty == ty -> Some (tk.location, advance p_state)
  | _ -> None
;;

let newline_parser p_state = simple_parse_gen Token.Newline p_state

let whitespace_parser p_state =
  match current p_state with
  | Some { ty = Token.Whitespace _; location } -> Some (Ast.Text location, advance p_state)
  | _ -> None
;;

let text_parser p_state =
  match current p_state with
  | Some { ty = Token.Text _; location } -> Some (Ast.Text location, advance p_state)
  | _ -> None
;;

let text_like_parser p_state = (whitespace_parser |? text_parser) p_state

let consume_parser p_state =
  match current p_state with
  | Some tk -> Some (Ast.Text tk.location, advance p_state)
  | None -> None
;;

let all_fmt_tag_parser p_state =
  match current p_state with
  | Some { ty = Token.Star; location } -> Some ((location, Ast.Bold), advance p_state)
  | Some { ty = Token.Slash; location } -> Some ((location, Ast.Italics), advance p_state)
  | Some { ty = Token.Backtick; location } ->
    Some ((location, Ast.Monospace), advance p_state)
  | Some { ty = Token.Tilde; location } ->
    Some ((location, Ast.StrikeThrough), advance p_state)
  | Some { ty = Token.Underscore; location } ->
    Some ((location, Ast.Underline), advance p_state)
  | _ -> None
;;

let fmt_tag_parser fmt p_state =
  match current p_state with
  | Some tk when tk.ty == Ast.fmt_to_token fmt -> Some (tk.location, advance p_state)
  | _ -> None
;;

let link_parser p_state =
  let open Ast in
  let lbrace_parser = simple_parse_gen Token.LSquareB in
  let rbrace_parser = simple_parse_gen Token.RSquareB in
  let lparen_parser = simple_parse_gen Token.LParen in
  let rparen_parser = simple_parse_gen Token.RParen in
  (* Parses text between two delimiters i.e. () or [] *)
  let inner_parser llimit rlimit p_state =
    match llimit p_state with
    | Some (open_brace, p_state) ->
      let (text, end_brace), p_state = (consume_parser >! rlimit) p_state in
      let inner_length, text =
        List.fold_left_map
          (fun len text ->
             let location = Ast.content_location text in
             len + location.length, location)
          0
          text
      in
      let end_brace =
        match end_brace with
        | Some end_brace -> end_brace
        | None ->
          (* Get a substitute location for the end brace*)
          if List.length text > 1
          then List.hd @@ List.rev text
          else if List.length text > 0
          then List.hd text
          else open_brace
      in
      let inner_offset = open_brace.offset + 1 in
      Some
        ( { open_delim = open_brace
          ; inner = { length = inner_length; offset = inner_offset }
          ; close_delim = end_brace
          }
        , p_state )
    | None -> None
  in
  match lbrace_parser p_state with
  | None -> None
  | Some (open_brace, p_state) ->
    (* Case 1: Link first *)
    (match inner_parser lbrace_parser rbrace_parser p_state with
     | Some (address, p_state) ->
       let name, p_state =
         match inner_parser lparen_parser rparen_parser p_state with
         | Some (res, p_state) -> Some res, p_state
         | None -> None, p_state
       in
       (* Parse the right brace *)
       (match rbrace_parser p_state with
        | None -> None
        | Some (close_brace, p_state) ->
          let open Token.Location in
          let location =
            match name, address with
            | Some name, address ->
              let length =
                open_brace.length
                + name.open_delim.length
                + name.inner.length
                + name.close_delim.length
                + address.open_delim.length
                + address.inner.length
                + address.close_delim.length
                + close_brace.length
              in
              let offset = open_brace.offset in
              { offset; length }
            | None, address ->
              let length =
                open_brace.length
                + address.open_delim.length
                + address.inner.length
                + address.close_delim.length
                + close_brace.length
              in
              let offset = open_brace.offset in
              { offset; length }
          in
          Some (Ast.Link { open_brace; address; name; close_brace; location }, p_state))
     | None ->
       (* Case 2: name first *)
       (match inner_parser lparen_parser rparen_parser p_state with
        | None -> None
        | Some (name, p_state) ->
          (match (inner_parser lbrace_parser rbrace_parser >> rbrace_parser) p_state with
           | None -> None
           | Some ((address, close_brace), p_state) ->
             let open Token.Location in
             let location =
               let length =
                 open_brace.length
                 + name.open_delim.length
                 + name.inner.length
                 + name.close_delim.length
                 + address.open_delim.length
                 + address.inner.length
                 + address.close_delim.length
                 + close_brace.length
               in
               let offset = open_brace.offset in
               { offset; length }
             in
             Some
               ( Ast.Link { open_brace; address; name = Some name; close_brace; location }
               , p_state ))))
;;

let rec contents_parser p_state =
  let fmt_or_prev_parser prev p_state =
    match fmt_parser p_state with
    | Some (ast, p_state) -> Some (prev :: ast, p_state)
    | None -> Some ([ prev ], p_state)
  in
  (* Case 1: Whitespace before formatted text *)
  match whitespace_parser p_state with
  | Some (whitespace, p_state) ->
    fmt_or_prev_parser whitespace p_state (* Case 2: Newline before formatted text *)
  | None ->
    (match to_text newline_parser p_state with
     | Some (newline, p_state) ->
       fmt_or_prev_parser newline p_state (* Case 3: Text at beginning *)
     | None ->
       (match beginning_check p_state with
        | None ->
          (* Regular parsing *)
          (match (link_parser |? text_parser |? consume_parser) p_state with
           | Some (res, p_state) -> Some ([ res ], p_state)
           | None -> None)
        | Some (_, p_state) ->
          (match fmt_parser p_state with
           | Some (ast, p_state) -> Some (ast, p_state)
           | None ->
             (match (link_parser |? text_parser |? consume_parser) p_state with
              | Some (res, p_state) -> Some ([ res ], p_state)
              | None -> None))))

and fmt_parser p_state =
  (* Parse fmt tags *)
  let fmt_tags, p_state = repeat all_fmt_tag_parser p_state in
  (* Parses the inside of the format tags *)
  let fmt_inner_content_parser p_state open_fmt prev_content =
    let open_fmt, fmt = open_fmt in
    (* Get content in tags *)
    let (content, end_fmt), p_state = (contents_parser >! fmt_tag_parser fmt) p_state in
    (* If there was a chained tag then it produced some content before we parsed
      out text, so we add that to our contents *)
    let content =
      match prev_content with
      | None -> Array.of_list @@ List.flatten content
      | Some prev_content -> Array.of_list @@ (prev_content :: List.flatten content)
    in
    (* Get the location of the contents as an [area]*)
    let content_length, content_loc =
      Array.fold_left_map
        (fun len content ->
           let location = Ast.content_location content in
           len + location.length, location)
        0
        content
    in
    (* Figure out an end fmt if we can't find one *)
    let end_fmt =
      match end_fmt with
      | Some end_fmt -> end_fmt
      | None ->
        if Array.length content > 1
        then Array.get content_loc (Array.length content_loc - 1)
        else if Array.length content > 0
        then Array.get content_loc 0
        else open_fmt
    in
    (* Our ast node *)
    let length = open_fmt.length + content_length + end_fmt.length in
    let offset = open_fmt.offset in
    let ast =
      Ast.Fmt { open_fmt; content; end_fmt; fmt_ty = fmt; location = { length; offset } }
    in
    Some (ast, p_state)
  in
  (* Parses each fmt tag *)
  let rec driver fmt_tags p_state =
    match fmt_tags with
    | [] -> None
    | hd :: tl ->
      (* Get formatted content of previous tag *)
      let prev_content = driver tl p_state in
      (match prev_content with
       | None -> fmt_inner_content_parser p_state hd None
       | Some (prev_content, p_state) ->
         fmt_inner_content_parser p_state hd (Some prev_content))
  in
  (* Check its a valid start tag *)
  match inverse whitespace_parser p_state with
  | None ->
    (* We consume the tags so they can't be used to end existing fmts *)
    let fmt_tags =
      List.map
        (fun fmt_tag ->
           let range, _ = fmt_tag in
           Ast.Text range)
        fmt_tags
    in
    Some (fmt_tags, p_state)
  | Some _ ->
    (match driver fmt_tags p_state with
     | Some (ast, p_state) -> Some ([ ast ], p_state)
     | None -> None)
;;

(** Parse some data *)
let parse source =
  let tokens = Dynarray.to_array @@ Lexer.lex_text source in
  let parser = init tokens in
  let rec driver p_state =
    match contents_parser p_state with
    | Some (contents, p_state) -> contents @ driver p_state
    | None -> []
  in
  driver parser
;;
