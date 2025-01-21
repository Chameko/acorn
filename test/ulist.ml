open Common

let%expect_test "Unordered list" =
  test "- Apple\n- Milk";
  [%expect {|
    Ast.Green.UList at 0
                        Token.Dash at 0..1
                        (Token.Whitespace " ") at 1..2
                        (Token.Text "Apple") at 2..7
                        Token.Newline at 7..8
                        Token.Dash at 8..9
                        (Token.Whitespace " ") at 9..10
                        (Token.Text "Milk") at 10..14
    end at 14
    |}]

let%expect_test "Multi level unordered list" =
  test "- Family\n-- Mom\n-- Dad\n- Groceries\n -- Apple\n-- Milk";
  [%expect {|
    Ast.Green.UList at 0
                        Token.Dash at 0..1
                        (Token.Whitespace " ") at 1..2
                        (Token.Text "Family") at 2..8
                        Token.Newline at 8..9
                        Token.Dash at 9..10
                        Token.Dash at 10..11
                        (Token.Whitespace " ") at 11..12
                        (Token.Text "Mom") at 12..15
                        Token.Newline at 15..16
                        Token.Dash at 16..17
                        Token.Dash at 17..18
                        (Token.Whitespace " ") at 18..19
                        (Token.Text "Dad") at 19..22
                        Token.Newline at 22..23
                        Token.Dash at 23..24
                        (Token.Whitespace " ") at 24..25
                        (Token.Text "Groceries") at 25..34
                        Token.Newline at 34..35
                        (Token.Whitespace " ") at 35..36
                        Token.Dash at 36..37
                        Token.Dash at 37..38
                        (Token.Whitespace " ") at 38..39
                        (Token.Text "Apple") at 39..44
                        Token.Newline at 44..45
                        Token.Dash at 45..46
                        Token.Dash at 46..47
                        (Token.Whitespace " ") at 47..48
                        (Token.Text "Milk") at 48..52
    end at 52
    |}]

let%expect_test "Missing space" =
  test "-Family";
  [%expect {|
    Token.Dash at 0..1
    (Token.Text "Family") at 1..7
    |}]

let%expect_test "Text before and after ulist" =
  test "Hello\n- Bob\n- Anby\nHow are you?";
  [%expect {|
    (Token.Text "Hello") at 0..5
    Token.Newline at 5..6
    Ast.Green.UList at 6
                        Token.Dash at 6..7
                        (Token.Whitespace " ") at 7..8
                        (Token.Text "Bob") at 8..11
                        Token.Newline at 11..12
                        Token.Dash at 12..13
                        (Token.Whitespace " ") at 13..14
                        (Token.Text "Anby") at 14..18
                        Token.Newline at 18..19
    end at 19
    (Token.Text "How") at 19..22
    (Token.Whitespace " ") at 22..23
    (Token.Text "are") at 23..26
    (Token.Whitespace " ") at 26..27
    (Token.Text "you?") at 27..31
    |}]
