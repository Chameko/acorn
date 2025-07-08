let%expect_test "lexer range" =
  Format.printf
    "%a"
    (Format.pp_print_array Acorn.Token.pp)
    (Dynarray.to_array @@ Acorn.Lexer.lex_text "Hello world");
  [%expect
    {|
    { Token.ty = (Token.Text "Hello");
      location = { Token.start = 1; stop = 5; line = 1 } }
    { Token.ty = (Token.Whitespace " ");
      location = { Token.start = 6; stop = 6; line = 1 } }
    { Token.ty = (Token.Text "world");
      location = { Token.start = 7; stop = 11; line = 1 } }
    |}]
;;
