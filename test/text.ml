open Common

let text_setup source =
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (reconstruct_content source))
    (setup source)
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
