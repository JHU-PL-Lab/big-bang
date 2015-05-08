open Batteries;;
open Tiny_bang_ast_pretty;;

let toploop_operate e =
  print_string (pretty_expr e);
  print_string "\n";
  flush stdout
;;

let () =
  Tiny_bang_parser.parse_tiny_bang_expressions IO.stdin
    |> LazyList.map fst
    |> LazyList.iter toploop_operate
;;
