open Common

let text_setup source =
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (reconstruct_content source))
    (setup source)
;;

let%expect_test "text1" =
  text_setup "Hello world";
  [%expect
    {|
    Text |Hello| Text | |
    Text |world|
    |}]
;;
