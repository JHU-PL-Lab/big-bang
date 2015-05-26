(**
  A front-end for the LittleBang parser library.
*)

open Batteries;;

open Little_bang_ast;;
open Little_bang_generated_lexer;;
open Little_bang_generated_parser;;
open Tiny_bang_parser_support;;

let parse_little_bang_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    begin
      reset_ast_position_hash();
      let result =
        Little_bang_generated_parser.delim_expr
          Little_bang_generated_lexer.token
          buf
      in
      let position_hash = Tiny_bang_parser_support.get_ast_position_hash () in
      match result with
        | Some(e) -> Some(e,position_hash)
        | None -> None
    end
  in
  LazyList.from_while read_expr;;

let parse_little_bang_program (input : IO.input) =
  let buf = Lexing.from_input input in
  reset_ast_position_hash();
  let e = Little_bang_generated_parser.prog
            Little_bang_generated_lexer.token
            buf
  in
  let position_hash = Tiny_bang_parser_support.get_ast_position_hash() in
  (e,position_hash)
;;
