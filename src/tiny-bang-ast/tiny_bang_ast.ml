(**
   Contains data type definitions for the TinyBang AST.
*)

open Batteries;;
open Tiny_bang_ast_uid;;
open Tiny_bang_utils;;

(** A module for hashtables keyed by UIDs. *)
module Ast_uid_hashtbl = Tiny_bang_ast_uid.Ast_uid_hashtbl;;

(** A data type for identifiers in TinyBang. *)
type ident = Ident of string | Fresh_ident of int;;

module Ident_hash =
struct
  type t = ident
  let equal = (=)
  let hash = Hashtbl.hash
end
;;

module Ident_hashtbl = Hashtbl.Make(Ident_hash);;

module Ident_order =
struct
  type t = ident
  let compare = compare
end
;;

module Ident_set = Set.Make(Ident_order);;

let fresh_ident_counter = ref 0;;

let new_fresh_ident () =
  let current_fresh_ident = !fresh_ident_counter in
  fresh_ident_counter := current_fresh_ident + 1;
  Fresh_ident current_fresh_ident
;;

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

module Var_hash =
struct
  type t = var
  let equal = var_equal
  let hash = var_hash
end;;

module Var_hashtbl = Hashtbl.Make(Var_hash);;

module Var_order =
struct
  type t = var
  let compare = var_compare
end;;

module Var_set = Set.Make(Var_order);;

module Var_map = Map.Make(Var_order);;

(** Individual pattern filters. *)
type pattern_filter =
  | Empty_filter of ast_uid
  | Label_filter of ast_uid * label * var
  | Conjunction_filter of ast_uid * var * var
  | Int_filter of ast_uid * var
  | Ref_filter of ast_uid * var  
;;

(** Sets of pattern filter rules that comprise a pattern. *)
type pattern_filter_rules = pattern_filter Var_map.t;;

(** The builtin in Tinybang. **)
type builtin_op = 
  | Op_plus   (* + *)
  | Op_ref    (* := *)
;;

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
  | Builtin_redex of ast_uid * builtin_op * var list 

(** TinyBang's value type. *)
and	value =
  | Empty_onion_value of ast_uid
  | Label_value of ast_uid * label * var
  | Onion_value of ast_uid * var * var
  | Function_value of ast_uid * pattern * expr
  | Int_value of ast_uid * int
  | Ref_value of ast_uid * var
  ;;
