(**
  This module contains resources used by the Menhir generated parser which
  are then manipulated by the parser facade.  It is not to be used
  directly.
*)
    
open Tiny_bang_ast;;
open Tiny_bang_parser_types;;

let ast_position_hash : file_region Ast_uid_hashtbl.t ref
  = ref (Ast_uid_hashtbl.create(10));;

let reset_ast_position_hash () =
  ast_position_hash := Ast_uid_hashtbl.create(10);;

