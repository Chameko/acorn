open Acorn

let rec reconstruct_content source ppf content =
  match content with
  | Ast.Text t -> Format.fprintf ppf "Text |%s|" (Ast.extract_location source t)
  | Ast.Fmt fmt ->
    Format.fprintf
      ppf
      "@[<hov>Fmt {@ open: |%s|@ content: %a@ end: |%s|@ formatting: |%a|@ }@]"
      (Ast.extract_location source fmt.open_fmt)
      (Format.pp_print_array ~pp_sep:Format.pp_print_space (reconstruct_content source))
      fmt.content
      (Ast.extract_location source fmt.end_fmt)
      Ast.pp_formatting
      fmt.fmt_ty
  | Ast.Link lnk ->
    let delimit_content_pp ppf dc =
      let open Ast in
      Format.fprintf
        ppf
        "@[<hov>{@ open_delim: |%s|@ inner: |%s|@ close_delim: |%s|@ }]"
        (Ast.extract_location source dc.open_delim)
        (Ast.extract_location source dc.inner)
        (Ast.extract_location source dc.close_delim)
    in
    Format.fprintf
      ppf
      "@[<hov>Link {@ open: |%s|@ address: %a@ name: %a@ close: |%s|@ }@]"
      (Ast.extract_location source lnk.open_brace)
      delimit_content_pp
      lnk.address
      (Format.pp_print_option delimit_content_pp)
      lnk.name
      (Ast.extract_location source lnk.close_brace)
  | _ -> Format.fprintf ppf "Unsupported :("
;;
