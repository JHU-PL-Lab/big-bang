(**
   Contains data type definitions for the LittleBang AST.
*)

open Tiny_bang_ast_uid;;
open Tiny_bang_utils;;

type label = Tiny_bang_ast.label;;

type ident =
  | Ident of string
  | Fresh_ident of int
;;

let new_fresh_ident () =
  match Tiny_bang_ast.new_fresh_ident () with
  | Tiny_bang_ast.Fresh_ident n -> Fresh_ident n
  | Tiny_bang_ast.Ident _ ->
    raise @@ Invariant_failure
      "Tiny_bang_ast.new_fresh_ident returned Ident (not Fresh_ident)"
  | Tiny_bang_ast.Builtin_ident _ ->
    raise @@ Invariant_failure
      "Tiny_bang_ast.new_fresh_ident returned Builtin_ident (not Fresh_ident)"      
;;

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
