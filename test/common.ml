open Acorn

let setup source =
  let tokens = Acorn.Lexer.parse_text source in
  let parser = Acorn.Parser.init (Array.of_list tokens) in
  let rec driver p_state =
    match Parser.contents_parser p_state with
    | Some (contents, p_state) -> contents @ driver p_state
    | None -> []
  in
  driver parser
;;

let rec reconstruct_content source ppf content =
  match content with
  | Ast.Text t -> Format.fprintf ppf "Text |%s|" (Ast.extract_area source t)
  | Ast.Fmt fmt ->
    Format.fprintf
      ppf
      "@[<hov>Fmt {@ open: |%s|@ content: %a@ end: |%s|@ formatting: |%a|@ }@]"
      (Token.extract_range source fmt.open_fmt)
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (reconstruct_content source))
      fmt.content
      (Token.extract_range source fmt.end_fmt)
      Ast.pp_formatting
      fmt.fmt_ty
  | Ast.Link lnk ->
    let delimit_content_pp ppf dc =
      let open Ast in
      Format.fprintf ppf "@[<hov>{@ open_delim: |%s|@ inner: |%s|@ close_delim: |%s|@ }]"
        (Token.extract_range source dc.open_delim)
        (Ast.extract_area source dc.inner)
        (Token.extract_range source dc.close_delim)
    in
    Format.fprintf
      ppf
      "@[<hov>Link {@ open: |%s|@ address: %a@ name: %a@ close: |%s|@ }@]"
      (Token.extract_range source lnk.open_brace)
      (delimit_content_pp) lnk.address
      (Format.pp_print_option delimit_content_pp) lnk.name
      (Token.extract_range source lnk.close_brace)
  | _ -> Format.fprintf ppf "Unsupported :("
;;
