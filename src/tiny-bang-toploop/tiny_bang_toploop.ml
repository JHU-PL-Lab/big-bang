open Batteries;;

open Tiny_bang_ast_pretty;;
open Tiny_bang_ast_wellformedness;;
open Tiny_bang_interpreter;;
open Tiny_bang_logger;;
open Tiny_bang_toploop_options;;
open Tiny_bang_typechecker;;

type toploop_options = {
  toploop_typechecker_disabled : bool
};;

let toploop_operate toploop_options e =
  print_string "\n";
  begin
    try
      check_wellformed_expr e; 
      let do_eval =
        (if (not @@ toploop_options.toploop_typechecker_disabled) then true else
         if typecheck e
         then true
         else (print_string "Type error.\n"; false)
        )
      in
      if do_eval 
      then
        let v,env = eval e in
        print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n");
      else ()
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

let command_line_parsing () = 
  let parser = BatOptParse.OptParser.make ~version:"version 0.3" () in
  BatOptParse.OptParser.add parser
    ~long_name:"log"
    logging_option;
  BatOptParse.OptParser.add parser
    ~short_name:'T'
    ~long_name:"no-typecheck"
    disable_typechecking_option;
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] ->
    { toploop_typechecker_disabled =
        Option.default false @@
          disable_typechecking_option.BatOptParse.Opt.option_get()
    }
  | _ -> failwith "BAD!" (* TODO: better error message *)
;;

let () =
  (* parse command-line arguments *)
  let toploop_options = command_line_parsing () in

  print_string "TinyBang 0.3 Toploop\n";
  print_string "--------------------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Tiny_bang_parser.parse_tiny_bang_expressions IO.stdin
  |> LazyList.map fst
  |> LazyList.iter (toploop_operate toploop_options)
;;
