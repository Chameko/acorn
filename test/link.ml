open Common

let%expect_test "Simple [[]()] Link" =
  test "[[www.github.com](github)]";
  [%expect {|
    Ast.Green.Link at 0
                       Token.LSquareB at 0..1
                       Ast.Green.Url at 1
                                         Token.LSquareB at 1..2
                                         (Token.Text "www") at 2..5
                                         Token.Dot at 5..6
                                         (Token.Text "github") at 6..12
                                         Token.Dot at 12..13
                                         (Token.Text "com") at 13..16
                                         Token.RSquareB at 16..17
                       end at 17
                       Ast.Green.Name at 17
                                           Token.LParen at 17..18
                                           (Token.Text "github") at 18..24
                                           Token.RParen at 24..25
                       end at 25
                       Token.RSquareB at 25..26
    end at 26
    |}]

let%expect_test "Simple [()[]] Link" =
  test "[(github)[www.github.com]]";
  [%expect {|
    Ast.Green.Link at 0
                       Token.LSquareB at 0..1
                       Ast.Green.Name at 1
                                          Token.LParen at 1..2
                                          (Token.Text "github") at 2..8
                                          Token.RParen at 8..9
                       end at 9
                       Ast.Green.Url at 9
                                         Token.LSquareB at 9..10
                                         (Token.Text "www") at 10..13
                                         Token.Dot at 13..14
                                         (Token.Text "github") at 14..20
                                         Token.Dot at 20..21
                                         (Token.Text "com") at 21..24
                                         Token.RSquareB at 24..25
                       end at 25
                       Token.RSquareB at 25..26
    end at 26
    |}]

let%expect_test "Link in text" =
  test "This is the website: [[www.github.com](github)]. WOW!";
  [%expect {|
    (Token.Text "This") at 0..4
    (Token.Whitespace " ") at 4..5
    (Token.Text "is") at 5..7
    (Token.Whitespace " ") at 7..8
    (Token.Text "the") at 8..11
    (Token.Whitespace " ") at 11..12
    (Token.Text "website:") at 12..20
    (Token.Whitespace " ") at 20..21
    Ast.Green.Link at 21
                        Token.LSquareB at 21..22
                        Ast.Green.Url at 22
                                           Token.LSquareB at 22..23
                                           (Token.Text "www") at 23..26
                                           Token.Dot at 26..27
                                           (Token.Text "github") at 27..33
                                           Token.Dot at 33..34
                                           (Token.Text "com") at 34..37
                                           Token.RSquareB at 37..38
                        end at 38
                        Ast.Green.Name at 38
                                            Token.LParen at 38..39
                                            (Token.Text "github") at 39..45
                                            Token.RParen at 45..46
                        end at 46
                        Token.RSquareB at 46..47
    end at 47
    Token.Dot at 47..48
    (Token.Whitespace " ") at 48..49
    (Token.Text "WOW!") at 49..53
    |}]

let%expect_test "Link in emphasised text" =
  test "This is the website: /[[www.github.com](github)]./ WOW!";
  [%expect {|
    (Token.Text "This") at 0..4
    (Token.Whitespace " ") at 4..5
    (Token.Text "is") at 5..7
    (Token.Whitespace " ") at 7..8
    (Token.Text "the") at 8..11
    (Token.Whitespace " ") at 11..12
    (Token.Text "website:") at 12..20
    (Token.Whitespace " ") at 20..21
    Ast.Green.Italics at 21
                           Token.Slash at 21..22
                           Ast.Green.Link at 22
                                               Token.LSquareB at 22..23
                                               Ast.Green.Url at 23
                                                                  Token.LSquareB at 23..24
                                                                  (Token.Text
                                                                     "www") at 24..27
                                                                  Token.Dot at 27..28
                                                                  (Token.Text
                                                                     "github") at 28..34
                                                                  Token.Dot at 34..35
                                                                  (Token.Text
                                                                     "com") at 35..38
                                                                  Token.RSquareB at 38..39
                                               end at 39
                                               Ast.Green.Name at 39
                                                                   Token.LParen at 39..40
                                                                   (Token.Text
                                                                      "github") at 40..46
                                                                   Token.RParen at 46..47
                                               end at 47
                                               Token.RSquareB at 47..48
                           end at 48
                           Token.Dot at 48..49
                           Token.Slash at 49..50
                           (Token.Whitespace " ") at 50..51
                           (Token.Text "WOW!") at 51..55
    end at 55
    |}]

let%expect_test "Link with nested URL" =
  test "[[www.github.com/[wacky[weird]]](github)]";
  [%expect {|
    Ast.Green.Link at 0
                       Token.LSquareB at 0..1
                       Ast.Green.Url at 1
                                         Token.LSquareB at 1..2
                                         (Token.Text "www") at 2..5
                                         Token.Dot at 5..6
                                         (Token.Text "github") at 6..12
                                         Token.Dot at 12..13
                                         (Token.Text "com") at 13..16
                                         Token.Slash at 16..17
                                         Token.LSquareB at 17..18
                                         (Token.Text "wacky") at 18..23
                                         Token.LSquareB at 23..24
                                         (Token.Text "weird") at 24..29
                                         Token.RSquareB at 29..30
                                         Token.RSquareB at 30..31
                                         Token.RSquareB at 31..32
                       end at 32
                       Ast.Green.Name at 32
                                           Token.LParen at 32..33
                                           (Token.Text "github") at 33..39
                                           Token.RParen at 39..40
                       end at 40
                       Token.RSquareB at 40..41
    end at 41
    |}]

let%expect_test "Link with nested name" =
  test "[[www.github.com](github (the good one (like really good)))]";
  [%expect {|
    Ast.Green.Link at 0
                       Token.LSquareB at 0..1
                       Ast.Green.Url at 1
                                         Token.LSquareB at 1..2
                                         (Token.Text "www") at 2..5
                                         Token.Dot at 5..6
                                         (Token.Text "github") at 6..12
                                         Token.Dot at 12..13
                                         (Token.Text "com") at 13..16
                                         Token.RSquareB at 16..17
                       end at 17
                       Ast.Green.Name at 17
                                           Token.LParen at 17..18
                                           (Token.Text "github") at 18..24
                                           (Token.Whitespace " ") at 24..25
                                           Token.LParen at 25..26
                                           (Token.Text "the") at 26..29
                                           (Token.Whitespace " ") at 29..30
                                           (Token.Text "good") at 30..34
                                           (Token.Whitespace " ") at 34..35
                                           (Token.Text "one") at 35..38
                                           (Token.Whitespace " ") at 38..39
                                           Token.LParen at 39..40
                                           (Token.Text "like") at 40..44
                                           (Token.Whitespace " ") at 44..45
                                           (Token.Text "really") at 45..51
                                           (Token.Whitespace " ") at 51..52
                                           (Token.Text "good") at 52..56
                                           Token.RParen at 56..57
                                           Token.RParen at 57..58
                                           Token.RParen at 58..59
                       end at 59
                       Token.RSquareB at 59..60
    end at 60
    |}]

let%expect_test "Incorrect link 1" =
  test "stuff before [[www.github.com]ohno(github)] stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    Token.LSquareB at 14..15
    (Token.Text "www") at 15..18
    Token.Dot at 18..19
    (Token.Text "github") at 19..25
    Token.Dot at 25..26
    (Token.Text "com") at 26..29
    Token.RSquareB at 29..30
    (Token.Text "ohno") at 30..34
    Token.LParen at 34..35
    (Token.Text "github") at 35..41
    Token.RParen at 41..42
    Token.RSquareB at 42..43
    (Token.Whitespace " ") at 43..44
    (Token.Text "stuff") at 44..49
    (Token.Whitespace " ") at 49..50
    (Token.Text "after") at 50..55
    |}]

let%expect_test "Incorrect link 2" =
  test "stuff before [ohno[www.github.com](github)] stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    (Token.Text "ohno") at 14..18
    Token.LSquareB at 18..19
    (Token.Text "www") at 19..22
    Token.Dot at 22..23
    (Token.Text "github") at 23..29
    Token.Dot at 29..30
    (Token.Text "com") at 30..33
    Token.RSquareB at 33..34
    Token.LParen at 34..35
    (Token.Text "github") at 35..41
    Token.RParen at 41..42
    Token.RSquareB at 42..43
    (Token.Whitespace " ") at 43..44
    (Token.Text "stuff") at 44..49
    (Token.Whitespace " ") at 49..50
    (Token.Text "after") at 50..55
    |}]

let%expect_test "Incorrect link 3" =
  test "stuff before [[www.github.com](github)ohno] stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    Token.LSquareB at 14..15
    (Token.Text "www") at 15..18
    Token.Dot at 18..19
    (Token.Text "github") at 19..25
    Token.Dot at 25..26
    (Token.Text "com") at 26..29
    Token.RSquareB at 29..30
    Token.LParen at 30..31
    (Token.Text "github") at 31..37
    Token.RParen at 37..38
    (Token.Text "ohno") at 38..42
    Token.RSquareB at 42..43
    (Token.Whitespace " ") at 43..44
    (Token.Text "stuff") at 44..49
    (Token.Whitespace " ") at 49..50
    (Token.Text "after") at 50..55
    |}]

let%expect_test "Missing end URL B" =
  test "stuff before [[www.github.com(github)] stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    Token.LSquareB at 14..15
    (Token.Text "www") at 15..18
    Token.Dot at 18..19
    (Token.Text "github") at 19..25
    Token.Dot at 25..26
    (Token.Text "com") at 26..29
    Token.LParen at 29..30
    (Token.Text "github") at 30..36
    Token.RParen at 36..37
    Token.RSquareB at 37..38
    (Token.Whitespace " ") at 38..39
    (Token.Text "stuff") at 39..44
    (Token.Whitespace " ") at 44..45
    (Token.Text "after") at 45..50
    |}]

let%expect_test "Missing end Name B" =
  test "stuff before [[www.github.com](github] stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    Token.LSquareB at 14..15
    (Token.Text "www") at 15..18
    Token.Dot at 18..19
    (Token.Text "github") at 19..25
    Token.Dot at 25..26
    (Token.Text "com") at 26..29
    Token.RSquareB at 29..30
    Token.LParen at 30..31
    (Token.Text "github") at 31..37
    Token.RSquareB at 37..38
    (Token.Whitespace " ") at 38..39
    (Token.Text "stuff") at 39..44
    (Token.Whitespace " ") at 44..45
    (Token.Text "after") at 45..50
    |}]

let%expect_test "Missing end B" =
  test "stuff before [[www.github.com](github) stuff after";
  [%expect {|
    (Token.Text "stuff") at 0..5
    (Token.Whitespace " ") at 5..6
    (Token.Text "before") at 6..12
    (Token.Whitespace " ") at 12..13
    Token.LSquareB at 13..14
    Token.LSquareB at 14..15
    (Token.Text "www") at 15..18
    Token.Dot at 18..19
    (Token.Text "github") at 19..25
    Token.Dot at 25..26
    (Token.Text "com") at 26..29
    Token.RSquareB at 29..30
    Token.LParen at 30..31
    (Token.Text "github") at 31..37
    Token.RParen at 37..38
    (Token.Whitespace " ") at 38..39
    (Token.Text "stuff") at 39..44
    (Token.Whitespace " ") at 44..45
    (Token.Text "after") at 45..50
    |}]

let%expect_test "Emphasis in title" =
  test "[[www.github.com](the *github* )]";
  [%expect {|
    Ast.Green.Link at 0
                       Token.LSquareB at 0..1
                       Ast.Green.Url at 1
                                         Token.LSquareB at 1..2
                                         (Token.Text "www") at 2..5
                                         Token.Dot at 5..6
                                         (Token.Text "github") at 6..12
                                         Token.Dot at 12..13
                                         (Token.Text "com") at 13..16
                                         Token.RSquareB at 16..17
                       end at 17
                       Ast.Green.Name at 17
                                           Token.LParen at 17..18
                                           (Token.Text "the") at 18..21
                                           (Token.Whitespace " ") at 21..22
                                           Ast.Green.Bold at 22
                                                               Token.Star at 22..23
                                                               (Token.Text
                                                                  "github") at 23..29
                                                               Token.Star at 29..30
                                           end at 30
                                           (Token.Whitespace " ") at 30..31
                                           Token.RParen at 31..32
                       end at 32
                       Token.RSquareB at 32..33
    end at 33
    |}]
