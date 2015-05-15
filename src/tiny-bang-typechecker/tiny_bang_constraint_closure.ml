open Batteries;;

open Tiny_bang_application_matching_types;;
open Tiny_bang_application_matching;;
open Tiny_bang_types;;
open Tiny_bang_utils;;

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
                    | Constraint(Intermediate_lower_bound(a1),a2) ->
                        Some (a1,a2)
                    | _ -> None
        )
    (* For each one, find all applicable lower bounds. *)
    |> Enum.map
        (fun (a1,a2) ->
          let rts = Constraint_database.type_lower_bounds_of a1 cs in
          Enum.map (fun rt -> Constraint(Type_lower_bound(rt),a2)) rts
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
        (fun c -> match c with
                    | Constraint(Application_lower_bound(a1,a2),a3) ->
                        Some(a1,a2,a3)
                    | _ -> None
        )
    (* For each one, perform the appropriate application matching check.  Then,
       freshen the variables and such. *)
    |> Enum.map
        (fun (a1,a2,a3) ->
          application_match a1 a2 cs
            |> Application_matching_result_set.enum
            |> Enum.map
                (fun (Application_matching_result(result)) ->
                  match result with
                    | None ->
                        raise (Not_yet_implemented "close_by_application")
                    | Some(a',cs1',cs1'') ->
                        raise (Not_yet_implemented "close_by_application")
                )
        )
    (* And bring them alll together. *)
    |> Enum.concat
    |> Constraint_database.of_enum
;;
