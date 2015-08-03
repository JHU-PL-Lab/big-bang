open Batteries;;

let rec step () =
  print_string "\n";
  print_string "Please enter expressions followed by \";;\" to evaluate.\n";
  print_string "\n";
  flush stdout;
  let program_option =
    IO.stdin
    |> Lexing.from_input
    |> Big_bang_parser.toploop_program Big_bang_lexer.read
  in
  match program_option with
  | Some program ->
    let tiny_bang_expression =
      program
      |> Big_bang_translator.translate_program
      |> Little_bang_a_translator.a_translate
    in
    begin
      try
        Tiny_bang_ast_wellformedness.check_wellformed_expr tiny_bang_expression;
        begin
          if Tiny_bang_typechecker.typecheck tiny_bang_expression then
            let variable, environment = Tiny_bang_interpreter.eval tiny_bang_expression in
            print_string @@
            Tiny_bang_ast_pretty.pretty_var variable ^
            " where " ^
            Tiny_bang_interpreter.pretty_env environment ^
            "\n"
          else
            print_string "Type error.\n"
        end
      with
      | Tiny_bang_ast_wellformedness.Illformedness_found(illformednesses) ->
        print_string "Provided expression is ill-formed:\n";
        illformednesses
        |> List.iter
          (
            fun illformedness ->
              print_string @@
              "   " ^
              Tiny_bang_ast_wellformedness.pretty_illformedness illformedness ^
              "\n"
          )
    end;
    step ()
  | None ->
    ()
;;

let () =
  print_string "Big Bang 0.3 Toploop\n";
  print_string "--------------------\n";
  step ()
;;
