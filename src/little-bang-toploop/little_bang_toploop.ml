open Batteries;;

open Little_bang_a_translator;;
open Tiny_bang_ast_pretty;;
open Tiny_bang_ast_wellformedness;;
open Tiny_bang_interpreter;;
open Tiny_bang_typechecker;;

let toploop_operate e =
  let e = a_translate e in
  print_string "\n";
  begin
    try
      check_wellformed_expr e;
      (if typecheck e
        then
          let v,env = eval e in
          print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n");
        else
          print_string "Type error.\n");
    with
      | Illformedness_found(ills) ->
          print_string "Provided expression is ill-formed:\n";
          List.iter
            (fun ill ->
              print_string @@ "   " ^ pretty_illformedness ill ^ "\n")
            ills
  end;
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout
;;

let () =
  print_string "LittleBang 0.3 Toploop\n";
  print_string "--------------------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Little_bang_parser.parse_little_bang_expressions IO.stdin
    |> LazyList.map fst
    |> LazyList.iter toploop_operate
;;
