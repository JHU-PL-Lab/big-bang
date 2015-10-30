(**
   Contains data type definitions for the TinyBang AST.
*)

open Batteries;;
open Tiny_bang_ast_uid;;
open Tiny_bang_utils;;

(** A module for hashtables keyed by UIDs. *)
module Ast_uid_hashtbl = Tiny_bang_ast_uid.Ast_uid_hashtbl;;

(** The builtin in Tinybang. **)
type builtin_op =
  | Op_int_plus      (** int+ *)
  | Op_int_minus     (** int- *)
  | Op_int_times     (** int* *)
  | Op_int_equal     (** int= *)
  | Op_int_lessthan  (** int< *)
  | Op_ref           (** := *)
  | Op_array_new     (** arrayNew *)
  | Op_array_length  (** arrayLength *)
  | Op_array_get     (** arrayGet *)
  | Op_array_set     (** arraySet *)
;;

(** {6 Identifiers} *)

(** A data type for identifiers in TinyBang. *)
type ident =
  | Ident of string
  | Fresh_ident of int
  | Builtin_ident of builtin_op * int
  | Builtin_local_ident of builtin_op * ident * int
;;

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

(** {6 Variables} *)

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack = Freshening_stack of ident list;;

let empty_freshening_stack = Freshening_stack [];;

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

(** {6 Patterns} *)

(** Pattern variables, which are distinct because they are not instantiated. *)
type pvar = Pvar of ast_uid * ident;;

let pvar_compare (Pvar(_,i1)) (Pvar(_,i2)) = compare i1 i2;;

let pvar_equal x1 x2 = pvar_compare x1 x2 = 0;;

module Pvar_order =
struct
  type t = pvar
  let compare = pvar_compare
end;;

module Pvar_set = Set.Make(Pvar_order);;

module Pvar_map = Map.Make(Pvar_order);;

(** An injector from pattern variable to expression variable.  The resulting
    variable has no freshening stack. *)
let var_of_pvar (Pvar(uid,i)) = Var(uid,i,None);;

(** Individual pattern filters. *)
type pattern_filter =
  | Empty_filter of ast_uid
  | Label_filter of ast_uid * label * pvar
  | Conjunction_filter of ast_uid * pvar * pvar
  | Int_filter of ast_uid * pvar
  | Ref_filter of ast_uid * pvar
  | Array_filter of ast_uid * pvar
;;
(* Note that the variables on Int_filter and Ref_filter are of a different sort;
   they have different binding rules. *)

(** Sets of pattern filter rules that comprise a pattern. *)
type pattern_filter_rules = pattern_filter Pvar_map.t;;

(** The type of a TinyBang pattern. *)
type pattern = Pattern of ast_uid * pvar * pattern_filter_rules;;

(** {6 Expressions} *)

(** The type of a TinyBang expression. *)
type expr = Expr of ast_uid * clause list

(** The type of a clause within a TinyBang expression. *)
and clause = Clause of ast_uid * var * redex

(** The type of a reducable expression. *)
and redex =
  | Value_redex of ast_uid * value
  | Var_redex of ast_uid * var
  | Appl_redex of ast_uid * var * var
  | Builtin_redex of ast_uid * builtin_op * var list

(** TinyBang's value type. *)
and value =
  | Empty_onion_value of ast_uid
  | Label_value of ast_uid * label * var
  | Onion_value of ast_uid * var * var
  | Function_value of ast_uid * pattern * expr
  | Int_value of ast_uid * int
  (*NOTE: just like `var's pointing to a ref may be re-bound to different values*)
  | Array_value of ast_uid * var array
  | Ref_value of ast_uid * var
;;
