open Batteries;;

let () =
  Big_bang_abstract_toploop.start
    "Big Bang"
    "0.3"
    Big_bang_lexer.read
    Big_bang_parser.toploop_program
    (
      fun big_bang_program ->
        big_bang_program
        |> Big_bang_translator.translate_program
        |> Little_bang_a_translator.a_translate
    )
;;
