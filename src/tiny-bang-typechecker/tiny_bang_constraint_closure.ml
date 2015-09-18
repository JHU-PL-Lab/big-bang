open Batteries;;

open Tiny_bang_application_matching;;
open Tiny_bang_application_matching_types;;
open Tiny_bang_ast;;
open Tiny_bang_compatibility;;
open Tiny_bang_contours;;
open Tiny_bang_nondeterminism;;
open Tiny_bang_types;;
open Tiny_bang_types_pretty;;
open Tiny_bang_utils;;

(* ************************************************************************** *)
(* LOGGER *)

let logger = Tiny_bang_logger.make_logger "Tiny_bang_constraint_closure";;

(* ************************************************************************** *)
(* UTILITY FUNCTIONS *)
let select_sensible_lower_bounds a cs =
  let open Nondeterminism_monad in
  let%bind rt = pick_enum @@ Constraint_database.type_lower_bounds_of a cs in
  if sensible rt cs
  then return rt
  else zero ()
;;

(* ************************************************************************** *)
(* CLOSURE RULES *)
(* Functions which embody the closure rules in the specification. *)

(**
   Performs transitivity closure steps on a given constraint set.
   @param cs The constraint set over which to perform the closure.
   @return The constraints learned from this process (which do not include the
          original constraints).
*)
let close_by_transitivity (cs : Constraint_database.t) : tbconstraint Enum.t =
  let constraints_m =
    let open Nondeterminism_monad in
    (* Find an intermediate constraint. *)
    let%bind c = pick_enum @@ Constraint_database.enum cs in
    let%bind (a1,a2) =
      match c with
      | Lower_bound_constraint(Intermediate_lower_bound(a1),a2) ->
        return (a1,a2)
      | _ ->
        zero ()
    in
    (* Pick a suitable lower bound. *)
    let%bind rt = pick_enum @@ Constraint_database.type_lower_bounds_of a1 cs in
    (* Create a transitive constraint. *)
    return @@ Lower_bound_constraint(Type_lower_bound(rt),a2)
  in
  Nondeterminism_monad.enum constraints_m
;;

(**
   Performs application closure on a given constraint set.
   @param cs The constraint set on which to perform closure.
   @return The constraints learned from this process (which do not include the
          original constraints).
*)
let close_by_application (cs : Constraint_database.t)
  : tbconstraint Enum.t =
  let constriants_enum_m =
    let open Nondeterminism_monad in
    (* Find an application. *)
    let%bind c = pick_enum @@ Constraint_database.enum cs in
    let%bind (a0,a1,a2) =
      match c with
      | Lower_bound_constraint(Application_lower_bound(a0,a1),a2) ->
        return (a0,a1,a2)
      | _ ->
        zero ()
    in
    (* Perform the appropriate matching check and then freshen variables. *)
    let%bind (Application_matching_result(result)) =
      pick_enum @@ application_match a0 a1 cs
    in
    match result with
    | None ->
      return @@ Enum.singleton @@ Inconsistency_constraint
    | Some(a1',cs1',cs1'') ->
      (* We have a result.  Build the variable replacement function. *)
      let Tvar(i,cntr_option) = a2 in
      let cntr =
        begin
          match cntr_option with
          | Some cntr -> cntr
          | None -> raise @@
            Invariant_failure(
              "Attempt to polyinstantiate type variable " ^ pretty_tvar a1' ^
              " without contour!")
        end
      in
      let cntr' = extend cntr i in
      let body_bound_vars = Constraint_database.bound_variables_of cs1' in
      let binding_vars = cs1'' |> List.map (fun (_,a) -> a) in
      let bound_vars =
        Tvar_set.union body_bound_vars @@ Tvar_set.of_list binding_vars
      in
      let repl_fn (Tvar(i,_) as a) =
        if Tvar_set.mem a bound_vars then Tvar(i,Some cntr') else a
      in
      (* Now that we have the replacement function, apply it. *)
      let cs2'' = cs1'' |> List.map (fun (rt,a) -> (rt, repl_fn a)) in
      let cs2' = cs1' |> Constraint_database.replace_variables repl_fn in
      let a2' = repl_fn a1' in
      return @@ Enum.concat @@ List.enum
        [ Constraint_database.enum cs2'
        ; cs2''
          |> List.enum
          |> Enum.map
            (fun (rt,a) -> Lower_bound_constraint(Type_lower_bound(rt),a))
        ; Enum.singleton
            (Lower_bound_constraint(Intermediate_lower_bound(a2'),a2))
        ]
  in
  constriants_enum_m
  |> Nondeterminism_monad.enum
  |> Enum.concat
;;

(**
   An abstract definition of a built-in closure.
   @param cs The constraint set on which to perform closure.
   @param op The operator to use in closure.
   @param f A function which, when given the concrete types for a built-in
            operation of this form, yields an enumeration of constraints.
   @return The constraints learned from this process (which do not include the
          original constraints).
*)
let close_primitive_builtin
      (cs : Constraint_database.t)
      (op : builtin_op)
      (f : tvar -> filtered_type list -> tbconstraint Enum.t)
  : tbconstraint Enum.t =
  (* TODO: this function could take a map from op to f; then, it would be able
           to close all primitives in one pass. *)
  let constraints_enum_m =
    let open Nondeterminism_monad in
    let%bind c = pick_enum @@ Constraint_database.enum cs in
    let%bind (operands,upper_bound) =
      match c with
      | Lower_bound_constraint(
          Builtin_lower_bound(op',operands),upper_bound) when op = op' ->
        return @@ (operands,upper_bound)
      | _ ->
        zero ()
    in
    let%bind operand_filtered_types =
      sequence @@ List.enum @@
        List.map (fun a -> select_sensible_lower_bounds a cs) operands
    in
    return @@ f upper_bound @@ List.of_enum operand_filtered_types
  in
  constraints_enum_m
  |> Nondeterminism_monad.enum
  |> Enum.concat
;;

(**
   Performs integer addition closure.
   @param cs The constraint set on which to perform closure.
   @return The constraints learned from this process (which do not include the
          original constraints).  
*)
let close_by_int_op op (cs : Constraint_database.t) : tbconstraint Enum.t =
  close_primitive_builtin cs op @@
  fun upper_bound operands ->
    match operands with
    | [Filtered_type(t1,_,_);Filtered_type(t2,_,_)] ->
      begin
        match t1,t2 with
        | Int_type,Int_type ->
          Enum.singleton @@
            Lower_bound_constraint(
              Type_lower_bound(unfiltered_type Int_type),
              upper_bound)
        | _ ->
          (* Operands must be ints. *)
          Enum.singleton @@ Inconsistency_constraint    
      end
    | _ ->
      (* There must be two operands. *)
      Enum.singleton @@ Inconsistency_constraint
;;

(**
   Performs integer equality closure.
   @param cs The constraint set on which to perform closure.
   @return The constraints learned from this process (which do not include the
          original constraints).  
*)
let close_by_int_bool_op op (cs : Constraint_database.t) : tbconstraint Enum.t =
  close_primitive_builtin cs op @@
  fun upper_bound operands ->
    match operands with
    | [Filtered_type(t1,_,_);Filtered_type(t2,_,_)] ->
      begin
        match t1,t2 with
        | Int_type,Int_type ->
          let empty_onion_tvar =
            Tvar(Builtin_ident(op,0),Some initial_contour)
          in
          let empty_onion_constraint = Enum.singleton @@
            Lower_bound_constraint(
              Type_lower_bound(unfiltered_type Empty_onion_type),
              empty_onion_tvar)
          in
          let label_constraints =
            ["True";"False"]
            |> List.enum
            |> Enum.map
              (fun name ->
                Lower_bound_constraint(
                  Type_lower_bound(unfiltered_type @@
                    Label_type(Label(Ident name), empty_onion_tvar)),
                upper_bound))
          in
          Enum.append empty_onion_constraint label_constraints
        | _ ->
          (* Operands must be ints. *)
          Enum.singleton @@ Inconsistency_constraint    
      end
    | _ ->
      (* There must be two operands. *)
      Enum.singleton @@ Inconsistency_constraint
;;

(* This exists to not be a total function if someone forgets to write a closure
   rule.*)
let builtin_closure op =
  match op with
  | Op_int_plus -> close_by_int_op Op_int_plus
  | Op_int_minus -> close_by_int_op Op_int_minus
  | Op_int_times -> close_by_int_op Op_int_times
  | Op_int_equal -> close_by_int_bool_op Op_int_equal
  | Op_int_lessthan -> close_by_int_bool_op Op_int_lessthan
  | Op_array_new -> fun (cs : Constraint_database.t) ->
      close_primitive_builtin cs Op_array_new @@ fun upper_bound operands ->
        begin
          match operands, upper_bound with
          | [Filtered_type(t1,_,_);initial_value_type], Tvar (tvar_ident, tvar_contour) ->
            begin
              match t1 with
              | Int_type ->
                let contents_type = Tvar(Builtin_local_ident (Op_array_new, tvar_ident, 0), tvar_contour)
                in
                BatList.enum [
                  Lower_bound_constraint(
                    Type_lower_bound(unfiltered_type @@ Array_type contents_type),
                    upper_bound);
                  Lower_bound_constraint(
                    Type_lower_bound(initial_value_type),
                    contents_type
                  );
                ]
              | _ ->
                (* Operand1 must be an int. *)
                Enum.singleton @@ Inconsistency_constraint
            end
          | _, _ ->
            (* There must be two operands. *)
            Enum.singleton @@ Inconsistency_constraint
        end
  | Op_array_length -> fun (cs : Constraint_database.t) ->
      close_primitive_builtin cs Op_array_length @@ fun upper_bound operands ->
        begin
          match operands with
          | [Filtered_type(t1,_,_)] ->
            begin
              match t1 with
              | Array_type _->
                Enum.singleton @@ Lower_bound_constraint(
                  Type_lower_bound(unfiltered_type Int_type),
                  upper_bound)
              | _ ->
                (* Operand1 must be an array. *)
                Enum.singleton @@ Inconsistency_constraint
            end
          | _ ->
            (* There must be one operand. *)
            Enum.singleton @@ Inconsistency_constraint
        end
  | Op_array_get -> fun (cs : Constraint_database.t) ->
      close_primitive_builtin cs Op_array_get @@ fun upper_bound operands ->
        begin
          match operands with
          | [Filtered_type(Array_type contents,_,_); Filtered_type(Int_type,_,_)] ->
            Enum.singleton @@
              Lower_bound_constraint (Intermediate_lower_bound contents, upper_bound)
          | _ ->
              Enum.singleton @@ Inconsistency_constraint
        end
    | Op_array_set -> fun (cs : Constraint_database.t) ->
      close_primitive_builtin cs Op_array_get @@ fun upper_bound operands ->
        begin
          match operands with
          | [Filtered_type(Array_type contents,_,_); Filtered_type(Int_type,_,_); newValue] ->
              BatList.enum @@ [
                Lower_bound_constraint (Type_lower_bound(unfiltered_type Empty_onion_type), upper_bound);
                Lower_bound_constraint (Type_lower_bound newValue, contents)
              ]
          | _ ->
              Enum.singleton @@ Inconsistency_constraint
        end

  | Op_ref -> raise (Failure "There aren't yet closure rules for Op_ref")

(* ************************************************************************** *)
(* CLOSURE *)
(* The mechanism by which a complete closure occurs. *)

(**
   Performs a complete deductive closure on the provided set of type system
   constraints.
*)
let rec perform_closure cs =
  let closure_functions =
    [ close_by_transitivity
    ; close_by_application
    ; builtin_closure Op_int_plus
    ; builtin_closure Op_int_minus
    ; builtin_closure Op_int_times
    ; builtin_closure Op_int_equal
    ; builtin_closure Op_int_lessthan
    ; builtin_closure Op_array_new
    ; builtin_closure Op_array_length
    ; builtin_closure Op_array_get
    ; builtin_closure Op_array_set
    ] in
  let cs' =
    Constraint_database.union cs @@ Constraint_database.of_enum @@
    (closure_functions
     |> List.enum
     |> Enum.map (fun fn -> fn cs)
     |> Enum.concat)
  in
  if Constraint_database.size cs <> Constraint_database.size cs'
  then perform_closure cs' else cs'
;;
