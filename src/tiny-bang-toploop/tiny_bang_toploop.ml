open Batteries;;

open Tiny_bang_ast_pretty;;
open Tiny_bang_interpreter;;

let toploop_operate e =
  print_string "\n";
  let v,env = eval e in
  print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n");
  print_string "\n";
  flush stdout
;;

let () =
  Tiny_bang_parser.parse_tiny_bang_expressions IO.stdin
    |> LazyList.map fst
    |> LazyList.iter toploop_operate
;;
