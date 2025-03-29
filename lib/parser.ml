(** The parser state *)
type t =
  { tokens : Token.t array
  ; cursor : int
  }

let init tokens = { tokens; cursor = 0 }

let check_number string =
  let regex = Re.Emacs.compile_pat "^[0-9]+$" in
  Re.execp regex string
;;

(** Get the current token *)
let current p_state =
  if p_state.cursor < Array.length p_state.tokens
  then Some p_state.tokens.(p_state.cursor)
  else None
;;

(** Advance the p_state *)
let advance p_state = { p_state with cursor = p_state.cursor + 1 }

(******************************************************************************)
(* Combinators *)
(******************************************************************************)

let alternate p1 p2 p_state =
  match p1 p_state with
  | None -> p2 p_state
  | res -> res
;;

let ( |? ) p1 p2 = alternate p1 p2

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
  | Some (range, p_state) -> Some (Ast.Text [ range ], p_state)
  | None -> None
;;

(******************************************************************************)
(* Parsers *)
(******************************************************************************)

let simple_parse_gen ty =
  fun p_state ->
  match current p_state with
  | Some tk when tk.ty == ty -> Some (tk.location, advance p_state)
  | _ -> None
;;

let newline_parser p_state = simple_parse_gen Token.Newline p_state

let whitespace_parser p_state =
  match current p_state with
  | Some { ty = Token.Whitespace _; location } ->
    Some (Ast.Text [ location ], advance p_state)
  | _ -> None
;;

let text_parser p_state =
  match current p_state with
  | Some { ty = Token.Text _; location } -> Some (Ast.Text [ location ], advance p_state)
  | _ -> None
;;

let text_like_parser p_state = (whitespace_parser |? text_parser) p_state

let consume_parser p_state =
  match current p_state with
  | Some tk -> Some (Ast.Text [ tk.location ], advance p_state)
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
          (match (text_parser |? consume_parser) p_state with
           | Some (res, p_state) -> Some ([ res ], p_state)
           | None -> None)
        | Some (_, p_state) ->
          (match fmt_parser p_state with
           | Some (ast, p_state) -> Some (ast, p_state)
           | None ->
             (match (text_parser |? consume_parser) p_state with
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
      | None -> List.flatten content
      | Some prev_content -> prev_content :: List.flatten content
    in
    (* Get the location of the contents as an [area]*)
    let content_loc = List.flatten @@ List.map Ast.content_location content in
    (* Figure out an end fmt if we can't find one *)
    let end_fmt =
      match end_fmt with
      | Some end_fmt -> end_fmt
      | None ->
        if List.length content > 1
        then List.hd @@ List.rev content_loc
        else if List.length content > 0
        then List.hd content_loc
        else open_fmt
    in
    (* Our ast node *)
    let location = Ast.flatten_ranges ([ open_fmt ] @ content_loc @ [ end_fmt ]) in
    let ast = Ast.Fmt { open_fmt; content; end_fmt; fmt_ty = fmt; location } in
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
           Ast.Text [ range ])
        fmt_tags
    in
    Some (fmt_tags, p_state)
  | Some _ ->
    (match driver fmt_tags p_state with
     | Some (ast, p_state) -> Some ([ ast ], p_state)
     | None -> None)
;;
