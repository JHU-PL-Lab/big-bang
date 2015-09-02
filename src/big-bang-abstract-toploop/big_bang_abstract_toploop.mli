exception Tiny_bang_translation_error of string
exception Illformed_program of string
exception Type_error of string
exception Interpretation_error of string

val start :
  ?well_formedness_checker:(Tiny_bang_ast.expr -> unit) ->
  ?typechecker:(Tiny_bang_ast.expr -> unit) ->
  ?interpreter:(Tiny_bang_ast.expr ->
                Tiny_bang_ast.var *
                Tiny_bang_ast.value Tiny_bang_interpreter.Environment.t) ->
  ?pretty_printer:(Tiny_bang_ast.var *
                   Tiny_bang_ast.value Tiny_bang_interpreter.Environment.t ->
                   string) ->
  string ->
  string ->
  'a ->
  ('a -> Batteries.Lexing.lexbuf -> 'b option) ->
  ('b -> Tiny_bang_ast.expr) -> unit
