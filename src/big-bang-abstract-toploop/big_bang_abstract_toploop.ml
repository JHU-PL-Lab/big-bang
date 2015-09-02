open Batteries;;

exception Tiny_bang_translation_error of string
exception Illformed_program of string
exception Type_error of string
exception Interpretation_error of string

let default_well_formedness_checker tiny_bang_program =
  try
    Tiny_bang_ast_wellformedness.check_wellformed_expr tiny_bang_program
  with
  | Tiny_bang_ast_wellformedness.Illformedness_found illformednesses ->
    raise @@ Illformed_program (
      illformednesses
      |> List.map Tiny_bang_ast_wellformedness.pretty_illformedness
      |> String.join ", "
    )
;;

let default_typechecker tiny_bang_program =
  if Tiny_bang_typechecker.typecheck tiny_bang_program then
    ()
  else
    raise @@ Type_error "Generic type error."
;;

let default_interpreter tiny_bang_program =
  Tiny_bang_interpreter.eval tiny_bang_program
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
    let program_option =
      IO.stdin
      |> Lexing.from_input
      |> parser lexer
    in
    match program_option with
    | Some program ->
      begin
        try
          let tiny_bang_program = tiny_bang_translator program in
          well_formedness_checker tiny_bang_program;
          typechecker tiny_bang_program;
          let result = interpreter tiny_bang_program in
          print_string @@ pretty_printer result
        with
        | Tiny_bang_translation_error error_message ->
          print_string @@ "Error translating to Tiny Bang " ^
                          "(this most likely indicates a bug on the implementation, " ^
                          "please report it on `https://github.com/JHU-PL-Lab/big-bang/issues'): " ^
                          "`" ^ error_message ^ "'."
        | Illformed_program error_message ->
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
  let banner =
    application_name ^ " " ^ version ^ " Toploop"
  in
  print_string @@
  banner ^ "\n" ^
  String.make (String.length banner) '-' ^ "\n";
  step ()
;;
