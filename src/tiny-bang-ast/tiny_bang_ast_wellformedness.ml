(**
   This module contains a number of routines to validate the well-formedness of
   a TinyBang AST.
*)

open Batteries;;
open Printf;;

open Tiny_bang_ast;;
open Tiny_bang_ast_pretty;;
open Tiny_bang_string_utils;;

type illformedness =
  | Filter_cycle of pvar list
  | Open_filter_variable of pvar
  | Duplicate_variable_binding of var
  | Open_expression_variable of var
;;

exception Illformedness_found of illformedness list;;

let pretty_illformedness ill =
  match ill with
  | Filter_cycle(xs) ->
    sprintf "Pattern variable cycle detected: %s"
      (pretty_list pretty_pvar xs)
  | Open_filter_variable(x) ->
    sprintf "Free variable detected in pattern: %s" (pretty_pvar x)
  | Duplicate_variable_binding(x) ->
    sprintf "Variable %s bound twice" (pretty_var x)
  | Open_expression_variable(x) ->
    sprintf "Variable %s is free in this expression" (pretty_var x)
;;

let merge_illformedness xs =
  let ills = 
    xs
    |> List.enum
    |> Enum.map
      (fun f ->
         try
           f (); []
         with
         | Illformedness_found(ills) -> ills)
    |> Enum.map List.enum
    |> Enum.concat
    |> List.of_enum
  in
  if not @@ List.is_empty ills
  then raise (Illformedness_found(ills))
;;

(**
   Asserts that a pattern is well-formed.  If the pattern is ill-formed, an
   [Illformedness_found] is raised.
   @param The pattern to check.
*)
let check_wellformed_pattern (Pattern(_,x_initial,pfm)) : unit =
  let rec walk_pattern x vars_seen_list vars_seen_set : unit =
    (* Make sure the variable exists. *)
    if not @@ Pvar_map.mem x pfm
    then
      raise (Illformedness_found([Open_filter_variable(x)]))
    else ();
    (* Check for cycles. *)
    if Pvar_set.mem x vars_seen_set
    then
      (* Identify the cycle by looking into the list for the variable. *)
      let (idx,_) = List.findi (fun _ el -> pvar_equal el x) vars_seen_list in
      let cycle = List.rev @@ (x :: List.take (idx+1) vars_seen_list) in
      raise (Illformedness_found([Filter_cycle(cycle)]))
    else ();
    (* Identify each of the variables in the pattern filter and explore those
       as well. *)
    let rec_vars =
      match Pvar_map.find x pfm with
      | Empty_filter(_) -> []
      | Array_filter(_) -> []
      | Label_filter(_,_,x') -> [x']
      | Conjunction_filter(_,x',x'') -> [x';x'']
      | Int_filter(_,_) -> []
      | Ref_filter(_,_) -> []
    in
    merge_illformedness @@
    List.map
      (fun x' -> function () ->
         walk_pattern x' (x::vars_seen_list) (Pvar_set.add x vars_seen_set))
      rec_vars
  in
  walk_pattern x_initial [] Pvar_set.empty
;;

(**
   Determines the variables bound by a pattern.  The pattern must be well-formed.
*)
let vars_bound_by_pattern (Pattern(_,x_initial,pfm)) : Pvar_set.t =
  let rec walk x =
    Pvar_set.add x @@
    match Pvar_map.find x pfm with
    | Empty_filter(_) -> Pvar_set.empty
    | Label_filter(_,_,x') -> walk x'
    | Conjunction_filter(_,x',x'') -> Pvar_set.union (walk x') (walk x'')
    | Int_filter(_,x') -> Pvar_set.singleton x' 
    | Array_filter(_,x') -> Pvar_set.singleton x' 
    | Ref_filter(_,x') -> Pvar_set.singleton x'
  in
  walk x_initial
;;

(**
   Determines the variables bound by an expression.
*)
let vars_bound_by_expr (Expr(_,cls)) =
  Var_set.of_list @@ List.map (fun (Clause(_,x,_)) -> x) cls
;;

(**
   Determines the variables free in an expression.
*)
let rec vars_free_in_expr (Expr(_,cls_initial)) =
  let rec walk cls =
    match cls with
    | [] -> Var_set.empty
    | (Clause(_,x,r))::t ->
      let free_t = walk t in
      let free_h =
        match r with
        | Value_redex(_,v) -> vars_free_in_value v
        | Var_redex(_,x') -> Var_set.singleton x'
        | Appl_redex(_,x1,x2) -> Var_set.of_list [x1;x2]
        | Builtin_redex(_,_,v_list) -> Var_set.of_list v_list
      in
      Var_set.remove x @@ Var_set.union free_h free_t
  in
  walk cls_initial
and vars_free_in_value v =
  match v with
  | Empty_onion_value(_) -> Var_set.empty
  | Int_value(_,_) -> Var_set.empty
  | Ref_value(_,x') -> Var_set.singleton x'
  | Label_value(_,_,x') -> Var_set.singleton x'
  | Onion_value(_,x1,x2) -> Var_set.of_list [x1;x2]
  | Array_value(_,_) -> Var_set.empty (*See comment at the definition*)
  | Function_value(_,p,e) ->
    Var_set.diff (vars_free_in_expr e)
      (vars_bound_by_pattern p
          |> Pvar_set.enum
          |> Enum.map var_of_pvar
          |> Var_set.of_enum)
;;

(**
   Determines if an expression is well-formed.
*)
let check_wellformed_expr e_initial : unit =
  let rec check_patterns_wellformed (Expr(_,cls_initial)) =
    merge_illformedness @@
    (cls_initial
     |> List.enum
     |> Enum.filter_map
       (fun (Clause(_,_,r)) ->
          match r with
          | Value_redex(_,Function_value(_,p,e)) ->
            Some(List.enum
                   [ function () -> check_wellformed_pattern p
                                  ; function () -> check_patterns_wellformed e
                   ])
          | _ -> None
       )
     |> Enum.concat
     |> List.of_enum)
  in
  let check_closed e =
    let free = vars_free_in_expr e in
    if Var_set.cardinal free > 0
    then raise (Illformedness_found(
        free |> Var_set.enum |> Enum.map (fun x -> Open_expression_variable(x))
        |> List.of_enum))
  in
  let check_unique_bindings (Expr(_,cls_initial)) =
    let merge_count_maps m1 m2 =
      let merge_fn _ n1o n2o =
        match (n1o,n2o) with
        | (Some n1, None) -> Some n1
        | (None, Some n2) -> Some n2
        | (Some n1, Some n2) -> Some (n1 + n2)
        | (None, None) -> None
      in
      Var_map.merge merge_fn m1 m2
    in
    let rec count_clause_bindings cls =
      cls
      |> List.enum
      |> Enum.map
        (fun (Clause(_,x,r)) ->
           let extras =
             match r with
             | Value_redex(_,Function_value(_,p,(Expr(_,cls')))) ->
               let pat_vars = vars_bound_by_pattern p in
               let pat_map =
                 pat_vars |> Pvar_set.enum
                 |> Enum.map (fun x -> (var_of_pvar x,1))
                 |> Var_map.of_enum
               in
               let body_map = count_clause_bindings cls' in
               merge_count_maps pat_map body_map
             | _ -> Var_map.empty
           in
           merge_count_maps extras @@ Var_map.singleton x 1
        )
      |> Enum.fold merge_count_maps Var_map.empty
    in
    let violations =
      count_clause_bindings cls_initial
      |> Var_map.enum
      |> Enum.filter_map
        (fun (x,n) -> if n > 1 then Some x else None)
      |> List.of_enum
    in
    if not @@ List.is_empty violations
    then raise (Illformedness_found(
        List.map (fun x -> Duplicate_variable_binding(x)) violations))
  in
  (* These must be done sequentially to satisfy invariants of the validation
     steps. *)
  check_patterns_wellformed e_initial;
  check_closed e_initial;
  check_unique_bindings e_initial
;;
