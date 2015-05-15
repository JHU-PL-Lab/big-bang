(** This module contains an implementation of application matching for the
    TinyBang type system. *)

open Batteries;;

open Tiny_bang_application_matching_types;;
open Tiny_bang_compatibility;;
open Tiny_bang_types;;
open Tiny_bang_utils;;

(**
  The type of an internal application matching result.  If the match fails,
  the second component is [None]; otherwise, it is [Some] triple between the
  return variable of the function, the body of the function, and the bindings
  which match the argument to the function's pattern.
*)
type internal_result =
  | Internal_result of Pattern_type_set.t * application_matching_result
;;

module Internal_result_ord =
struct
  type t = internal_result
  let compare = compare
end;;

module Internal_result_set = Set.Make(Internal_result_ord);;

(* TODO: detect and reject non-contractive function types *)

let application_match
      (fun_var_init : tvar)
      (arg_var : tvar)
      (cs : Constraint_set.t)
      : Application_matching_result_set.t =
  let rec app_match
            (fun_var : tvar)
            (pats_used : Pattern_type_set.t)
            : Internal_result_set.t =
    (* Determine the lower bounds of the function variable.  Throw away those
       which are not sensible, since every rule requires that check.  Then,
       discard the filters on that type (as we no longer care about them after
       the sensibility check). *)
    let typs = cs
                |> lower_bounds_of fun_var
                |> Enum.filter (fun rt -> sensible rt cs)
                |> Enum.map (fun (Filtered_type(t,_,_)) -> t)
    in
    (* For each such lower bound, process it appropriately, yielding a set of
       results.  Those sets will then be unioned. *)
    typs
      |> Enum.map
          (fun t ->
              (* Function lower bounds can be processed based on what concrete
                 type they have. *)
              match t with
                | Function_type(pat,ret_var,body) ->
                    (* Determine the ways in which the pattern is compatible
                       with the argument. *)
                    let compat_results = find_compatibility_cases
                                            arg_var
                                            cs
                                            pat
                                            pats_used
                    in
                    let pats_used' = Pattern_type_set.add pat pats_used in
                    (* Now adapt each of these results into a conclusion. *)
                    compat_results
                      |> List.enum
                      |> Enum.map
                            (fun bindings_opt ->
                              let payload =
                                match bindings_opt with
                                  | Some bindings -> 
                                      Application_matching_result(Some(
                                        ret_var,body,bindings))
                                  | None ->
                                      Application_matching_result None
                              in Internal_result(pats_used', payload)
                            )
                      |> Internal_result_set.of_enum
                | Onion_type(a1,a2) ->
                    (* Start by checking the left side. *)
                    let results = app_match a1 pats_used in
                    (* Then, for each result, a failure means we should pursue
                       the right side. *)
                    results
                      |> Internal_result_set.enum
                      |> Enum.map
                          (fun (Internal_result(pats, ans) as r) ->
                            match ans with
                              | Application_matching_result (Some _) ->
                                  (* This is a successful answer.  Put the
                                     result in its own set for unioning
                                     later. *)
                                  Internal_result_set.singleton r
                              | Application_matching_result None ->
                                  (* The match failed,  Use the visited patterns
                                     to explore the right side. *)
                                  app_match a2
                                    (Pattern_type_set.union pats_used pats)
                          )
                      |> Enum.fold
                          (fun acc -> fun v -> Internal_result_set.union acc v)
                          Internal_result_set.empty
                | _ ->
                    (* This is easy; there's only one answer to non-applicable
                       types. *)
                    Internal_result_set.singleton @@
                      Internal_result(Pattern_type_set.empty,
                        Application_matching_result None)
          )
      |> Enum.fold
            (fun acc -> fun v -> Internal_result_set.union acc v)
            Internal_result_set.empty
  in
  app_match fun_var_init Pattern_type_set.empty
    |> Internal_result_set.enum
    |> Enum.map (fun (Internal_result(_,result)) -> result)
    |> Application_matching_result_set.of_enum
;;
