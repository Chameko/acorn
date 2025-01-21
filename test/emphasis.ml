open Common

let%expect_test "Simple emphasis" =
  test "Something *bold*";
  [%expect {|
    (Token.Text "Something") at 0..9
    (Token.Whitespace " ") at 9..10
    Ast.Green.Bold at 10
                        Token.Star at 10..11
                        (Token.Text "bold") at 11..15
                        Token.Star at 15..16
    end at 16
    |}]

let%expect_test "Nested emphasis" =
  test "/*Nested*/";
  [%expect {|
    Ast.Green.Italics at 0
                          Token.Slash at 0..1
                          Ast.Green.Bold at 1
                                             Token.Star at 1..2
                                             (Token.Text "Nested") at 2..8
                                             Token.Star at 8..9
                          end at 9
                          Token.Slash at 9..10
    end at 10
    |}]

let%expect_test "Incorrectly nested emphasis" =
  test "/*Incorrect/*";
  [%expect {|
    Ast.Green.Italics at 0
                          Token.Slash at 0..1
                          Ast.Green.Bold at 1
                                             Token.Star at 1..2
                                             (Token.Text "Incorrect") at 2..11
                                             Token.Slash at 11..12
                                             Token.Star at 12..13
                          end at 13
    end at 13
    |}]

let%expect_test "No middle emphasis" =
  test "*Embolded*Words*";
  [%expect {|
    Ast.Green.Bold at 0
                       Token.Star at 0..1
                       (Token.Text "Embolded") at 1..9
                       Token.Star at 9..10
                       (Token.Text "Words") at 10..15
                       Token.Star at 15..16
    end at 16
    |}]

let%expect_test "No middle emphasis 2" =
  test "*Embold*ed";
  [%expect {|
    Ast.Green.Bold at 0
                       Token.Star at 0..1
                       (Token.Text "Embold") at 1..7
                       Token.Star at 7..8
                       (Token.Text "ed") at 8..10
    end at 10
    |}]

let%expect_test "No middle emphasis 3" =
  test "Emb*olded*";
  [%expect {|
    (Token.Text "Emb") at 0..3
    Token.Star at 3..4
    (Token.Text "olded") at 4..9
    Token.Star at 9..10
    |}]
