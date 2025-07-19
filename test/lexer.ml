let%expect_test "lexer range" =
  Format.printf
    "%a"
    (Format.pp_print_array Acorn.Token.pp)
    (Dynarray.to_array @@ Acorn.Lexer.lex_text "Hello world");
  [%expect
    {|
    { Token.ty = (Token.Text "Hello");
      location = { Token.Location.offset = 0; length = 5 } }
    { Token.ty = (Token.Whitespace " ");
      location = { Token.Location.offset = 5; length = 1 } }
    { Token.ty = (Token.Text "world");
      location = { Token.Location.offset = 6; length = 5 } }
    |}]
;;
