open Batteries;;

exception Tiny_bang_translation_error of string
exception Illformed_expression of string
exception Type_error of string
exception Interpretation_error of string

let default_well_formedness_checker tiny_bang_expression =
  try
    Tiny_bang_ast_wellformedness.check_wellformed_expr tiny_bang_expression
  with
  | Tiny_bang_ast_wellformedness.Illformedness_found illformednesses ->
    raise @@ Illformed_expression (
      illformednesses
      |> List.map Tiny_bang_ast_wellformedness.pretty_illformedness
      |> String.join ", "
    )
;;

let default_typechecker tiny_bang_expression =
  if Tiny_bang_typechecker.typecheck tiny_bang_expression then
    ()
  else
    raise @@ Type_error "Generic type error."
;;

let default_interpreter tiny_bang_expression =
  Tiny_bang_interpreter.eval tiny_bang_expression
;;

let default_pretty_printer (variable, environment) =
  Tiny_bang_ast_pretty.pretty_var variable ^
  " where " ^
  Tiny_bang_interpreter.pretty_env environment ^
  "\n"
;;

let start
    ?(well_formedness_checker = default_well_formedness_checker)
    ?(typechecker = default_typechecker)
    ?(interpreter = default_interpreter)
    ?(pretty_printer = default_pretty_printer)
    application_name
    version
    lexer
    parser
    tiny_bang_translator
  =
  let rec step () =
    print_string "\nPlease enter expressions followed by \";;\" to evaluate.\n\n";
    flush stdout;
    let expression_option =
      IO.stdin
      |> Lexing.from_input
      |> parser lexer
    in
    match expression_option with
    | Some expression ->
      begin
        try
          let tiny_bang_expression = tiny_bang_translator expression in
          well_formedness_checker tiny_bang_expression;
          typechecker tiny_bang_expression;
          let result = interpreter tiny_bang_expression in
          print_string @@ pretty_printer result
        with
        | Tiny_bang_translation_error error_message ->
          print_string @@ "Error translating to Tiny Bang " ^
                          "(this most likely indicates a bug on the implementation, " ^
                          "please report it on `https://github.com/JHU-PL-Lab/big-bang/issues'): " ^
                          "`" ^ error_message ^ "'."
        | Illformed_expression error_message ->
          print_string @@ "Illformed expression: " ^
                          "`" ^ error_message ^ "'."
        | Type_error error_message ->
          print_string @@ "Type error: " ^
                          "`" ^ error_message ^ "'."
        | Interpretation_error error_message ->
          print_string @@ "Interpretation error: " ^
                          "`" ^ error_message ^ "'."
      end;
      step ()
    | None -> ()
  in
  print_string @@
  application_name ^ " " ^ version ^ " Toploop\n" ^
  "--------------------\n";
  step ()
;;
