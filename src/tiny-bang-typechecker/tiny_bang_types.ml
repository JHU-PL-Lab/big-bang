open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_contours_types;;

(* TODO: consider adding (optional?) AST UIDs to these types to signify their
   origin (or some other type origin stuff). *)
  
type tvar =
  | Tvar of ident * contour
;;

module Tvar_order =
struct
  type t = tvar
  let compare = compare 
end;;

module Tvar_map = Map.Make(Tvar_order);;

type pattern_filter_type =
  | Empty_filter_type
  | Label_filter_type of label * tvar
  | Conjunction_filter_type of tvar * tvar
;;
  
type pattern_type =
  | Pattern_type of tvar * pattern_filter_type Tvar_map.t
;;

module Pattern_type_order =
struct
  type t = pattern_type
  let compare = compare
end;;

module Pattern_type_set = Set.Make(Pattern_type_order);;

module rec Types :
sig
  type tbconstraint =
    | Lower_bound_constraint of
        tbtype * Pattern_type_set.t * Pattern_type_set.t * tvar
    | Intermediate_constraint of tvar * tvar
    | Application_constraint of tvar * tvar * tvar
  and tbtype =
    | Empty_onion_type
    | Label_type of label * tvar
    | Onion_type of tvar * tvar
    | Function_type of pattern_type * tvar * Constraint_set.t
end = Types

and Constraint_order : Set.OrderedType =
  struct
    type t = Types.tbconstraint
    let compare = compare
  end

and Constraint_set : (Set.S with type elt = Types.tbconstraint) =
  Set.Make(Constraint_order)
;;

type tbconstraint = Types.tbconstraint;;
type tbtype = Types.tbtype;;
