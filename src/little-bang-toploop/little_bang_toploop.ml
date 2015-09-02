open Batteries;;

let () =
  Big_bang_abstract_toploop.start
    "Little Bang"
    "0.3"
    Little_bang_generated_lexer.token
    Little_bang_generated_parser.delim_expr
    (
      fun little_bang_program ->
        little_bang_program
        |> Little_bang_a_translator.a_translate
    )
;;
