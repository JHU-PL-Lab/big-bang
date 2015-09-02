open Batteries;;

let () =
  Big_bang_abstract_toploop.start
    "Tiny Bang"
    "0.3"
    Tiny_bang_lexer.token
    Tiny_bang_parser.delim_expr
    identity
;;
