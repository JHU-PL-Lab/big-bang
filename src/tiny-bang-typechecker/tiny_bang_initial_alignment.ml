open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_ast_pretty;;
open Tiny_bang_types;;

exception Align_error of string;;

let initial_align_var (x : var) : tvar =
  let (Var(_,i,fso)) = x in
  (* If fso is not None, then we're freshening something that shouldn't be
     freshened. *)
  if Option.is_some fso
  then raise (Align_error
                ("Cannot align freshened variable " ^ pretty_var x));
  (* Otherwise, create a type variable with no contour. *)
  Tvar(i,None)
;;

let initial_align_pvar (x : pvar) : tpvar =
  let (Pvar(_,i)) = x in
  Tpvar(i)
;;

let initial_align_pattern_filter pf =
  match pf with
  | Empty_filter(_) -> Empty_filter_type
  | Label_filter(_,l,x) -> Label_filter_type(l,initial_align_pvar x)
  | Conjunction_filter(_,x1,x2) ->
    Conjunction_filter_type(initial_align_pvar x1, initial_align_pvar x2)
  | Int_filter(_,x1) -> Int_filter_type(initial_align_pvar x1)
  | Array_filter(_,x1) -> Array_filter_type(initial_align_pvar x1)
  | Ref_filter(_,x1) -> Ref_filter_type(initial_align_pvar x1)
;;

let initial_align_pattern_filter_map pfm =
  Pvar_map.fold
    (fun x -> fun pf -> fun m ->
       Tpvar_map.add
         (initial_align_pvar x)
         (initial_align_pattern_filter pf)
         m)
    pfm
    Tpvar_map.empty
;;

let initial_align_pattern (Pattern(_,x,m)) =
  Pattern_type(initial_align_pvar x, initial_align_pattern_filter_map m)
;;

let rec initial_align_expr (Expr(_,cls)) =
  let (a_option,cs) =
    cls
    |> List.map initial_align_clause
    |> List.fold_left
      (fun (_,cs) -> fun (a,c) -> (Some a, Constraint_database.add c cs))
      (None, Constraint_database.empty)
  in
  match a_option with
  | None -> raise (Align_error "Cannot align empty expression")
  | Some a -> (a,cs)

and initial_align_clause cl =
  let (Clause(_,x,r)) = cl in
  let a = initial_align_var x in
  (a, Lower_bound_constraint(initial_align_redex r, a))

and initial_align_builtin_op op = op

and initial_align_redex r =
  match r with
  | Value_redex(_,v) ->
    Type_lower_bound(Filtered_type(
        initial_align_value v,
        Pattern_type_set.empty,
        Pattern_type_set.empty))
  | Var_redex(_,x) -> Intermediate_lower_bound(initial_align_var x)
  | Appl_redex(_,x1,x2) ->
    Application_lower_bound(initial_align_var x1,initial_align_var x2)
  | Builtin_redex(_,op,list_x) -> 
    Builtin_lower_bound(initial_align_builtin_op op,List.map initial_align_var list_x)

and initial_align_value v =
  match v with
  | Empty_onion_value(_) -> Empty_onion_type
  | Int_value(_,_) -> Int_type
  | Ref_value(_,x) -> Ref_type(initial_align_var x)
  | Array_value(_,_) -> raise @@ Align_error "Arrays are not literals"
  | Label_value(_,l,x) -> Label_type(l,initial_align_var x)
  | Onion_value(_,x1,x2) ->
    Onion_type(initial_align_var x1,initial_align_var x2)
  | Function_value(_,p,e) ->
    let (a,cs) = initial_align_expr e in
    Function_type(initial_align_pattern p, a, cs)
;;
