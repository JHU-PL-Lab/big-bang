(** This module contains an implementation of application matching for the
    TinyBang type system. *)

open Batteries;;

open Tiny_bang_application_matching_types;;
open Tiny_bang_compatibility;;
open Tiny_bang_nondeterminism
open Tiny_bang_types;;

(**
   The type of an internal application matching result.  If the match fails,
   the second component is [None]; otherwise, it is [Some] triple between the
   return variable of the function, the body of the function, and the bindings
   which match the argument to the function's pattern.
*)
type internal_result =
  | Internal_result of Pattern_type_set.t * application_matching_result
;;

(* TODO: detect and reject non-contractive function types *)

let application_match
    (fun_var_init : tvar)
    (arg_var : tvar)
    (cs : Constraint_database.t)
  : application_matching_result Enum.t =
  let rec app_match
      (fun_var : tvar)
      (pats_used : Pattern_type_set.t)
    : internal_result Nondeterminism_monad.m =
    let open Nondeterminism_monad in
    (* Pick a lower bound for the function variable. *)
    let%bind (Filtered_type(t,_,_) as rt) =
      pick_enum @@ Constraint_database.type_lower_bounds_of fun_var cs
    in
    (* Skip it if it is not sensible. *)
    let%bind () = stop_unless @@ sensible rt cs in
    (* Now branch based on the concrete type. *)
    match t with
    | Function_type(pat,ret_var,body) ->
      (* Determine a way in which the patter is compatible with the argument. *)
      let%bind compat_result =
        pick_enum @@ List.enum @@
        find_compatibility_cases arg_var cs pat pats_used
      in
      let pats_used' = Pattern_type_set.add pat pats_used in
      (* Adapt the result into a conclusion. *)
      let payload =
        match compat_result with
        | Some bindings ->
          Application_matching_result(Some(ret_var,body,bindings))
        | None ->
          Application_matching_result None
      in
      return @@ Internal_result(pats_used', payload)
    | Onion_type(a1,a2) ->
      (* Start by checking the left side. *)
      let%bind (Internal_result(pats, ans) as r) =
        app_match a1 pats_used
      in
      (* A failure means we should pursue the right side. *)
      begin
        match ans with
        | Application_matching_result (Some _) ->
          (* This is a successful answer; keep it. *)
          return r
        | Application_matching_result None ->
          (* The match failed.  Pick a result from the right side. *)
          app_match a2 (Pattern_type_set.union pats_used pats)
      end
    | _ ->
      (* This is easy; there's only one answer to non-applicable types. *)
      return @@ Internal_result(
        Pattern_type_set.empty,
        Application_matching_result None)
  in
  app_match fun_var_init Pattern_type_set.empty
  |> Nondeterminism_monad.enum
  |> Enum.map (fun (Internal_result(_,result)) -> result)
;;
