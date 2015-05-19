(**
  A front-end for the TinyBang parser library.
*)

open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_parser_support;;
open Tiny_bang_generated_lexer;;
open Tiny_bang_generated_parser;;

let parse_tiny_bang_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    begin
      reset_ast_position_hash();
      let result =
        Tiny_bang_generated_parser.delim_expr
          Tiny_bang_generated_lexer.token
          buf
      in
      let position_hash = Tiny_bang_parser_support.get_ast_position_hash () in
      match result with
        | Some(e) -> Some(e,position_hash)
        | None -> None
    end
  in
  LazyList.from_while read_expr;;

let parse_tiny_bang_program (input : IO.input) =
  let buf = Lexing.from_input input in
  reset_ast_position_hash();
  let e = Tiny_bang_generated_parser.prog
            Tiny_bang_generated_lexer.token
            buf
  in
  let position_hash = Tiny_bang_parser_support.get_ast_position_hash() in
  (e,position_hash)
;;
