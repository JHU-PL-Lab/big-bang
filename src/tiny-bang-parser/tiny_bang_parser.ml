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
  let last = ref false in
  let read_expr () =
    begin
      if !last then None else
        begin
          reset_ast_position_hash();
          let e,cont =
            Tiny_bang_generated_parser.delim_expr
              Tiny_bang_generated_lexer.token
              buf in
          last := cont;
          Some e
        end
    end
  in
  LazyList.from_while read_expr;;

