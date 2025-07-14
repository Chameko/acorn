open Common

let text_setup source =
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (reconstruct_content source))
    (Acorn.Parser.parse source)
;;

let%expect_test "text1_test" =
  text_setup "Hello world";
  [%expect
    {|
    Text |Hello| Text | |
    Text |world|
    |}]
;;

let%expect_test "fmt_test" =
  text_setup "*BOLD*";
  [%expect {| Fmt { open: |*| content: Text |BOLD| end: |*| formatting: |Ast.Bold| } |}]
;;

let%expect_test "fmt_space_test" =
  text_setup "* does not work*";
  [%expect
    {|
    Text |*| Text | | Text |does| Text | | Text |not| Text | | Text |work|
    Text |*|
    |}]
;;

let%expect_test "fmt_invalid_end" =
  text_setup "*does not work * works*";
  [%expect
    {|
    Fmt { open: |*| content: Text |does| Text | | Text |not| Text | | Text |work|
    Text | | Text |*| Text | | Text |works| end: |*| formatting: |Ast.Bold| }
    |}]
;;

let%expect_test "fmt_chain_test" =
  text_setup "*/Hello/*";
  [%expect
    {|
    Fmt { open: |*|
    content: Fmt { open: |/| content: Text |Hello| end: |/|
             formatting: |Ast.Italics| } end: |*| formatting: |Ast.Bold| }
    |}]
;;

let%expect_test "fmt_chain_test_2" =
  text_setup "*/Hello/ world*";
  [%expect
    {|
    Fmt { open: |*|
    content: Fmt { open: |/| content: Text |Hello| end: |/|
             formatting: |Ast.Italics| } Text | | Text |world| end: |*|
    formatting: |Ast.Bold| }
    |}]
;;

let%expect_test "fmt_link_test_1" =
  text_setup "[[github.com]]";
  [%expect
    {|
    Link { open: |[|
    address: { open_delim: |[| inner: |github.com| close_delim: |]| }] name:
             close: |]| }
    |}]
;;

let%expect_test "fmt_link_test_2" =
  text_setup "[[github.com](github)]";
  [%expect
    {|
    Link { open: |[|
    address: { open_delim: |[| inner: |github.com| close_delim: |]| }]
             name: { open_delim: |(| inner: |github| close_delim: |)| }]
                   close: |]| }
    |}]
;;

let%expect_test "fmt_link_test_3" =
  text_setup "[(github)[github.com]]";
  [%expect
    {|
    Link { open: |[|
    address: { open_delim: |[| inner: |github.com| close_delim: |]| }]
             name: { open_delim: |(| inner: |github| close_delim: |)| }]
                   close: |]| }
    |}]
;;

let%expect_test "fmt_link_test_bad_1" =
  text_setup "[[github.com] *Something*";
  [%expect
    {|
    Text |[| Text |[| Text |github| Text |.| Text |com| Text |]| Text | |
    Fmt { open: |*| content: Text |Something| end: |*| formatting: |Ast.Bold| }
    |}]
;;

let%expect_test "fmt_link_test_bad_2" =
  text_setup "[[github.com](github]";
  [%expect
    {|
    Text |[| Text |[| Text |github| Text |.| Text |com| Text |]| Text |(|
    Text |github|
    Text |]|
    |}]
;;

let%expect_test "fmt_link_test_bad_3" =
  text_setup "[(github)]";
  [%expect
    {|
    Text |[| Text |(| Text |github| Text |)|
    Text |]|
    |}]
;;
