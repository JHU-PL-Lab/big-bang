open Batteries;;

open Tiny_bang_application_matching;;
open Tiny_bang_application_matching_types;;
open Tiny_bang_contours;;
open Tiny_bang_contours_types;;
open Tiny_bang_types;;
open Tiny_bang_types_pretty;;
open Tiny_bang_utils;;

(* ************************************************************************** *)
(* LOGGER *)

let logger = Tiny_bang_logger.make_logger "Tiny_bang_constraint_closure";;

(* ************************************************************************** *)
(* CLOSURE RULES *)
(* Functions which embody the closure rules in the specification. *)

(**
  Performs transitivity closure steps on a given constraint set.
  @param cs The constraint set over which to perform the closure.
  @return The constraints learned from this process (which do not include the
          original constraints).
*)
let close_by_transitivity cs =
  cs
    |> Constraint_database.enum
    (* Find all the intermediate constraints *)
    |> Enum.filter_map
        (fun c -> match c with
                    | Lower_bound_constraint(Intermediate_lower_bound(a1),a2) ->
                        Some (a1,a2)
                    | _ -> None
        )
    (* For each one, find all applicable lower bounds. *)
    |> Enum.map
        (fun (a1,a2) ->
          let rts = Constraint_database.type_lower_bounds_of a1 cs in
          rts |> Enum.map
                    (fun rt -> Lower_bound_constraint(Type_lower_bound(rt),a2))
        )
    (* Now merge the enums to get the resulting stream. *)
    |> Enum.concat
    (* And now it should be a set. *)
    |> Constraint_database.of_enum
;;

(**
  Performs application closure on a given constraint set.
  @param cs The constraint set on which to perform closure.
  @return The constraints learned from this process (which do not include the
          original constraints).
*)
let close_by_application cs =
  cs
    |> Constraint_database.enum
    (* Find all of the applications. *)
    |> Enum.filter_map
        (fun c ->
          match c with
            | Lower_bound_constraint(Application_lower_bound(a1,a2),a3) ->
                Some(a1,a2,a3)
            | _ -> None
        )
    (* For each one, perform the appropriate application matching check.  Then,
       freshen the variables and such. *)
    |> Enum.map
        (fun (a0,a1,a2) ->
          application_match a0 a1 cs
            |> Application_matching_result_set.enum
            |> Enum.map
                (fun (Application_matching_result(result)) ->
                  match result with
                    | None ->
                        Enum.singleton Inconsistency_constraint
                    | Some(a1',cs1',cs1'') ->
                        (* We need to start by building the variable
                           substitution function. *)
                        let Tvar(i,cntr_option) = a2 in
                        let cntr =
                          begin
                            match cntr_option with
                              | Some cntr -> cntr
                              | None ->
                                  raise @@ Invariant_failure(
                                    "Attempt to polyinstantiate type " ^
                                    "variable " ^ pretty_tvar a1' ^
                                    " without contour!")
                          end
                        in
                        let cntr' = extend cntr i in
                        let body_bound_vars =
                          Constraint_database.bound_variables_of cs1' in
                        let binding_vars = cs1''
                          |> List.map (fun (_,a) -> a) in
                        let bound_vars = Tvar_set.union body_bound_vars @@
                          Tvar_set.of_list binding_vars in
                        let repl_fn (Tvar(i,_) as a) =
                          if Tvar_set.mem a bound_vars
                            then Tvar(i,Some cntr') else a
                        in
                        (* Now that we have the replacement function, apply
                           it. *)
                        let cs2'' = cs1'' |> List.map
                          (fun (rt,a) -> (rt, repl_fn a)) in
                        let cs2' = cs1' |>
                          Constraint_database.replace_variables repl_fn in
                        let a2' = repl_fn a1' in
                        Enum.concat @@ List.enum
                          [ Constraint_database.enum cs2'
                          ; cs2''
                              |> List.enum
                              |> Enum.map
                                  (fun (rt,a) ->
                                    Lower_bound_constraint(
                                      Type_lower_bound(rt),a))
                          ; Enum.singleton
                              (Lower_bound_constraint(
                                Intermediate_lower_bound(a2'),a2))
                          ]
                )
            |> Enum.concat
        )
    (* And bring them alll together. *)
    |> Enum.concat
    |> Constraint_database.of_enum
;;

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
    ] in
  let cs' =
    List.fold_left
      (fun cs1 fn ->
        let cs2 = fn cs1 in
        Constraint_database.union cs1 cs2)
      cs
      closure_functions
  in
  if Constraint_database.size cs <> Constraint_database.size cs'
    then perform_closure cs' else cs'
;;