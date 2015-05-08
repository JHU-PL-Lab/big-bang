(**
Contains data type definitions for the TinyBang AST.
*)

open Batteries;;

(** The type for the UIDs attached to TinyBang ASTs. *)
type ast_uid = Tiny_bang_ast_uid.ast_uid;;

(** A function for retrieving the next UID to use in a TinyBang AST. *)
let next_uid = Tiny_bang_ast_uid.next_uid;;

(** A function to extract the integer value of a UID. *)
let int_of_uid = Tiny_bang_ast_uid.int_of_uid;;

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

(** Individual pattern filters. *)
type pat_filter =
  | Empty_filter of ast_uid
  | Label_filter of ast_uid * label * var
  | Conjunction_filter of ast_uid * var * var;;

(** Pattern filter rules. *)
type pat_filter_rule = Pattern_filter_rule of ast_uid * var * pat_filter;;

(** Sets of pattern filter rules that comprise a pattern. *)
module Pattern_filter_rule_set = Set.Make(
  struct
    type t = pat_filter_rule
    let compare = compare
  end);;

(** The type of a TinyBang pattern. *)
type pattern = Pattern of ast_uid * var * Pattern_filter_rule_set.t;;

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
