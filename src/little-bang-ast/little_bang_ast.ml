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
  | Tiny_bang_ast.Fresh_ident(n, _) ->
        (*We're really only using this code to get at the number-generation
         * scheme, so we can ignore any labels.*)  
        Fresh_ident n
  | Tiny_bang_ast.Ident _ ->
    raise @@ Invariant_failure
      "Tiny_bang_ast.new_fresh_ident returned Ident (not Fresh_ident)"
  | Tiny_bang_ast.Builtin_ident _ ->
    raise @@ Invariant_failure
      "Tiny_bang_ast.new_fresh_ident returned Builtin_ident (not Fresh_ident)"
  | Tiny_bang_ast.Builtin_local_ident _ ->
    raise @@ Invariant_failure
      "Tiny_bang_ast.new_fresh_ident returned Builtin_local_ident (not Fresh_ident)"
;;

type var = Var of ast_uid * ident;;

type pattern =
  | Var_pattern of ast_uid * var
  | Empty_pattern of ast_uid
  | Label_pattern of ast_uid * label * pattern
  | Ref_pattern of ast_uid * pattern
  | Int_pattern of ast_uid * pattern
  | Array_pattern of ast_uid * pattern
  | Conjunction_pattern of ast_uid * pattern * pattern
;;

type expr =
  | Var_expr of ast_uid * var
  | Value_expr of ast_uid * value
  | Label_expr of ast_uid * label * expr
  | Onion_expr of ast_uid * expr * expr
  | Let_expr of ast_uid * var * expr * expr
  | Appl_expr of ast_uid * expr * expr
  | Builtin_expr of ast_uid * Tiny_bang_ast.builtin_op * expr list
  (* This is an expression in little bang, but a value in tiny bang.
     The reason why is that Ref_value in tiny bang requires a var,
     while Ref_expr in little bang takes an expression, so it needs to
     be able to add clauses, so it can't just be a litle bang value.*)
  | Ref_expr of ast_uid * expr
and value =
  | Empty_onion of ast_uid
  | Function of ast_uid * pattern * expr
  | Int_value of ast_uid * int
;;
