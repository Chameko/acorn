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

let rec repeat p1 limit p_state =
  match limit p_state with
  | Some (l, p_state) -> ([], Some l), p_state
  | None ->
    (match p1 p_state with
     | Some (res, p_state) ->
       let (tl, l), p_state = repeat p1 limit p_state in
       (res :: tl, l), p_state
     | None -> ([], None), p_state)
;;

let ( >! ) p1 limit = repeat p1 limit
let ( >> ) p1 p2 = sequence p1 p2

let ( let* ) p1 construct =
  match p1 with
  | Some res -> Some (construct res)
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

let consume_parser p_state = (whitespace_parser |? text_parser) p_state

let fmt_tag_parser_gen fmt =
  fun p_state ->
  let fmt_ty = Ast.fmt_to_token fmt in
  simple_parse_gen fmt_ty p_state
;;

let rec contents_parser p_state =
  (consume_parser
   |? fmt_parser_gen Ast.Bold
   |? fmt_parser_gen Ast.Italics
   |? fmt_parser_gen Ast.Monospace
   |? fmt_parser_gen Ast.StrikeThrough
   |? fmt_parser_gen Ast.Underline)
    p_state

and fmt_parser_gen fmt =
  let fmt_tag_parser = fmt_tag_parser_gen fmt in
  fun p_state ->
    match fmt_tag_parser p_state with
    | Some (open_fmt, p_state) ->
      let (content, end_fmt), p_state = (contents_parser >! fmt_tag_parser) p_state in
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
      let location = Ast.flatten_ranges ([ open_fmt ] @ content_loc @ [ end_fmt ]) in
      let ast = Ast.Fmt { open_fmt; content; end_fmt; fmt_ty = fmt; location } in
      Some (ast, p_state)
    | None -> None
;;
