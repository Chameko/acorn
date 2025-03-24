let%expect_test "lexer range" =
  Format.printf
    "%a"
    (Format.pp_print_list Acorn.Token.pp)
    (Acorn.Lexer.parse_text "Hello world");
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
