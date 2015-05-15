open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_contours_types;;

(* TODO: consider adding (optional?) AST UIDs to these types to signify their
   origin (or some other type origin stuff). *)
  
(* ************************************************************************** *)
(* DATA TYPES *)
(* The core data types which represent the grammar of the TinyBang type
   system. *) 
  
type tvar =
  | Tvar of ident * contour option
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
  type tbconstraint = Constraint of lower_bound * tvar
  and lower_bound =
    | Type_lower_bound of filtered_type
    | Intermediate_lower_bound of tvar
    | Application_lower_bound of tvar * tvar
  and filtered_type =
    | Filtered_type of tbtype * Pattern_type_set.t * Pattern_type_set.t
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

include Types;;

(* ************************************************************************** *)
(* UTILITIES *)
(* Functions for working with the above data types. *)

let lower_bounds_of tv cs =
  cs
    |> Constraint_set.enum
    |> Enum.filter_map
          (fun (Constraint(lb,tv')) ->
            match lb with
              | Type_lower_bound(ft) ->
                  if tv == tv' then Some ft else None
              | _ -> None)
;;
