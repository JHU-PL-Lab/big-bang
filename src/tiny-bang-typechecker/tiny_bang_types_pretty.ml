open Batteries;;
open Printf;;

open Tiny_bang_ast_pretty;;
open Tiny_bang_contours;;
open Tiny_bang_string_utils;;
open Tiny_bang_types;;

let pretty_tvar (Tvar(i,cntr_option)) =
  (pretty_ident i) ^
  match cntr_option with
  | Some cntr -> pretty_contour cntr
  | None -> "*"
;;

let pretty_tpvar (Tpvar(i)) = pretty_ident i;;

let pretty_pattern_filter_type pf =
  match pf with
  | Empty_filter_type -> "()"
  | Label_filter_type(l,a) -> pretty_label l ^ " " ^ pretty_tpvar a
  | Conjunction_filter_type(a1,a2) -> pretty_tpvar a1 ^ " * " ^ pretty_tpvar a2
  | Int_filter_type(a1) -> pretty_tpvar a1 ^ ":int"
  | Array_filter_type(a1) -> pretty_tpvar a1 ^ ":array"
  | Ref_filter_type(a1) -> "ref " ^ pretty_tpvar a1
;;

let pretty_pattern_type (Pattern_type(a,pfm)) =
  pretty_tpvar a ^ " \\ " ^
  (concat_sep_delim "{" "}" "; "
     (pfm
      |> Tpvar_map.enum
      |> Enum.map
        (fun (a,pf) ->
           pretty_tpvar a ^ " = " ^ pretty_pattern_filter_type pf
        )
     )
  )
;;

let pretty_pattern_type_set pts =
  concat_sep_delim "{" "}" ", "
    (pts |> Pattern_type_set.enum |> Enum.map pretty_pattern_type)
;;

let rec pretty_type t =
  match t with
  | Empty_onion_type -> "()"
  | Int_type -> "int"
  | Ref_type(a) -> "ref " ^ pretty_tvar a
  | Array_type(a) -> "array " ^ pretty_tvar a
  | Label_type(l,a) -> pretty_label l ^ " " ^ pretty_tvar a
  | Onion_type(a1,a2) -> pretty_tvar a1 ^ " & " ^ pretty_tvar a2
  | Function_type(p,a,cs) ->
    sprintf "%s -> %s\\%s"
      (pretty_pattern_type p)
      (pretty_tvar a)
      (pretty_constraints cs)

and pretty_constraints cs =
  concat_sep_delim "{" "}" ", "
    (cs |> Constraint_database.enum |> Enum.map pretty_constraint)

and pretty_constraint c =
  match c with
  | Lower_bound_constraint(lb,a) ->
    sprintf "%s <: %s" (pretty_lower_bound lb) (pretty_tvar a)
  | Inconsistency_constraint ->
    "inconsistent!"

and pretty_lower_bound lb =
  match lb with
  | Type_lower_bound(ft) -> pretty_filtered_type ft
  | Intermediate_lower_bound(a) -> pretty_tvar a
  | Application_lower_bound(a1,a2) -> pretty_tvar a1 ^ " " ^ pretty_tvar a2
  | Builtin_lower_bound(op, a_list) ->
    let pretty_tvar_list = List.map pretty_tvar a_list in
    pretty_builtin_op op ^
    List.reduce (fun x y -> " " ^ x ^ " " ^ y) pretty_tvar_list

and pretty_filtered_type (Filtered_type(t,pos,neg)) =
  pretty_type t ^
  (if Pattern_type_set.is_empty pos
   then "" else ("|+" ^ pretty_pattern_type_set pos)) ^
  (if Pattern_type_set.is_empty neg
   then "" else ("|-" ^ pretty_pattern_type_set neg))
;;

