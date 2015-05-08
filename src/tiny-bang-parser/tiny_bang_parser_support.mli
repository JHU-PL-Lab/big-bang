(**
This module contains resources used by the Menhir generated parser which
are then manipulated by the parser facade. It is not to be used
directly.
*)

open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_parser_types;;

val reset_ast_position_hash : unit -> unit

val get_ast_position_hash : unit -> file_region Ast_uid_hashtbl.t
