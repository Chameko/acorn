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
  test "- Family\n- - Mom\n- - Dad\n- Groceries\n- - Apple\n-- Milk";
  [%expect {|
    Ast.Green.UList at 0
                        Token.Dash at 0..1
                        (Token.Whitespace " ") at 1..2
                        (Token.Text "Family") at 2..8
                        Token.Newline at 8..9
                        Token.Dash at 9..10
                        (Token.Whitespace " ") at 10..11
                        Token.Dash at 11..12
                        (Token.Whitespace " ") at 12..13
                        (Token.Text "Mom") at 13..16
                        Token.Newline at 16..17
                        Token.Dash at 17..18
                        (Token.Whitespace " ") at 18..19
                        Token.Dash at 19..20
                        (Token.Whitespace " ") at 20..21
                        (Token.Text "Dad") at 21..24
                        Token.Newline at 24..25
                        Token.Dash at 25..26
                        (Token.Whitespace " ") at 26..27
                        (Token.Text "Groceries") at 27..36
                        Token.Newline at 36..37
                        Token.Dash at 37..38
                        (Token.Whitespace " ") at 38..39
                        Token.Dash at 39..40
                        (Token.Whitespace " ") at 40..41
                        (Token.Text "Apple") at 41..46
                        Token.Newline at 46..47
    end at 47
    Token.Dash at 47..48
    Token.Dash at 48..49
    (Token.Whitespace " ") at 49..50
    (Token.Text "Milk") at 50..54
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

let%expect_test "Ordered list 1" =
  test "1. First";
  [%expect {|
    Ast.Green.OList at 0
                        (Token.Text "1") at 0..1
                        Token.Dot at 1..2
                        (Token.Whitespace " ") at 2..3
                        (Token.Text "First") at 3..8
    end at 8
    |}]

let%expect_test "Ordered list 2" =
  test "~. First";
  [%expect {|
    Ast.Green.OList at 0
                        Token.Tilde at 0..1
                        Token.Dot at 1..2
                        (Token.Whitespace " ") at 2..3
                        (Token.Text "First") at 3..8
    end at 8
    |}]

let%expect_test "Multi level ordered list" =
  test "1. First\n~. ~. Counting again\n";
  [%expect {|
    Ast.Green.OList at 0
                        (Token.Text "1") at 0..1
                        Token.Dot at 1..2
                        (Token.Whitespace " ") at 2..3
                        (Token.Text "First") at 3..8
                        Token.Newline at 8..9
                        Token.Tilde at 9..10
                        Token.Dot at 10..11
                        (Token.Whitespace " ") at 11..12
                        Token.Tilde at 12..13
                        Token.Dot at 13..14
                        (Token.Whitespace " ") at 14..15
                        (Token.Text "Counting") at 15..23
                        (Token.Whitespace " ") at 23..24
                        (Token.Text "again") at 24..29
                        Token.Newline at 29..30
    end at 30
    |}]

let%expect_test "Text before and after olist" =
  test "Hello\n~. First\n2. Second\nHow are you?";
  [%expect {|
    (Token.Text "Hello") at 0..5
    Token.Newline at 5..6
    Ast.Green.OList at 6
                        Token.Tilde at 6..7
                        Token.Dot at 7..8
                        (Token.Whitespace " ") at 8..9
                        (Token.Text "First") at 9..14
                        Token.Newline at 14..15
                        (Token.Text "2") at 15..16
                        Token.Dot at 16..17
                        (Token.Whitespace " ") at 17..18
                        (Token.Text "Second") at 18..24
                        Token.Newline at 24..25
    end at 25
    (Token.Text "How") at 25..28
    (Token.Whitespace " ") at 28..29
    (Token.Text "are") at 29..32
    (Token.Whitespace " ") at 32..33
    (Token.Text "you?") at 33..37
    |}]

let%expect_test "olist numbers" =
  test "2342. Valid\n2342 2. Not valid";
  [%expect {|
    Ast.Green.OList at 0
                        (Token.Text "2342") at 0..4
                        Token.Dot at 4..5
                        (Token.Whitespace " ") at 5..6
                        (Token.Text "Valid") at 6..11
                        Token.Newline at 11..12
    end at 12
    (Token.Text "2342") at 12..16
    (Token.Whitespace " ") at 16..17
    (Token.Text "2") at 17..18
    Token.Dot at 18..19
    (Token.Whitespace " ") at 19..20
    (Token.Text "Not") at 20..23
    (Token.Whitespace " ") at 23..24
    (Token.Text "valid") at 24..29
    |}]

let%expect_test "olist missing space" =
  test "1.Not allowed";
  [%expect {|
    (Token.Text "1") at 0..1
    Token.Dot at 1..2
    (Token.Text "Not") at 2..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "allowed") at 6..13
    |}]
