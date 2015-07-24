(**
   Contains data type definitions for the LittleBang AST.
*)

open Tiny_bang_ast;;
open Tiny_bang_ast_uid;;

type var = Var of ast_uid * ident;;

type pattern =
  | Var_pattern of ast_uid * var
  | Empty_pattern of ast_uid
  | Label_pattern of ast_uid * label * pattern
  | Conjunction_pattern of ast_uid * pattern * pattern
;;

type expr =
  | Var_expr of ast_uid * var
  | Value_expr of ast_uid * value
  | Label_expr of ast_uid * label * expr
  | Onion_expr of ast_uid * expr * expr
  | Let_expr of ast_uid * var * expr * expr
  | Appl_expr of ast_uid * expr * expr
and value =
  | Empty_onion of ast_uid
  | Function of ast_uid * pattern * expr
;;
