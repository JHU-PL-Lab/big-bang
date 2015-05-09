(**
Contains data type definitions for the TinyBang AST.
*)

open Batteries;;
open Tiny_bang_ast_uid;;
open Tiny_bang_utils;;

(** A module for hashtables keyed by UIDs. *)
module Ast_uid_hashtbl = Tiny_bang_ast_uid.Ast_uid_hashtbl;;

(** A data type for identifiers in TinyBang. *)
type ident = Ident of string;;

(** The label type.  The identifier stored in this label does not contain the
    leading backtick. *)
type label = Label of ident;;

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack = Freshening_stack of ident list;;

(** Variables in the TinyBang AST. *)
type var = Var of ast_uid * ident * freshening_stack option;;

let var_compare (Var(_,i1,fso1)) (Var(_,i2,fso2)) =
  natural_compare_seq
    [ (fun () -> compare i1 i2)
    ; (fun () -> compare fso1 fso2)
    ];;

let var_hash (Var(_,i,fso)) =
  Hashtbl.hash i lxor Hashtbl.hash fso;;

let var_equal (Var(_,i1,fso1)) (Var(_,i2,fso2)) = i1 = i2 && fso1 = fso2;;

module VarOrder =
  struct
    type t = var
    let compare = var_compare
  end;;

module VarSet = Set.Make(VarOrder);;

module VarMap = Map.Make(VarOrder);;

(** Individual pattern filters. *)
type pat_filter =
  | Empty_filter of ast_uid
  | Label_filter of ast_uid * label * var
  | Conjunction_filter of ast_uid * var * var;;

(** Sets of pattern filter rules that comprise a pattern. *)
type pattern_filter_rules = pat_filter VarMap.t;;

(** The type of a TinyBang pattern. *)
type pattern = Pattern of ast_uid * var * pattern_filter_rules;;

(** The type of a TinyBang expression. *)
type expr = Expr of ast_uid * clause list

(** The type of a clause within a TinyBang expression. *)
and clause = Clause of ast_uid * var * redex

(** The type of a reducable expression. *)
and	redex =
  | Value_redex of ast_uid * value
  | Var_redex of ast_uid * var
  | Appl_redex of ast_uid * var * var

(** TinyBang's value type. *)
and	value =
  | Empty_onion_value of ast_uid
  | Label_value of ast_uid * label * var
  | Onion_value of ast_uid * var * var
  | Function_value of ast_uid * pattern * expr;;
