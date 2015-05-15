open Batteries;;

open Tiny_bang_application_matching_types;;
open Tiny_bang_application_matching;;
open Tiny_bang_types;;

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
    |> Constraint_set.of_enum
;;
