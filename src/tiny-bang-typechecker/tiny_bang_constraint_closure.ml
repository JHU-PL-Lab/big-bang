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
    |> Constraint_set.enum
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
          cs
            |> Constraint_set.enum
            |> Enum.filter_map
                (fun c ->
                  match c with
                    | Constraint(Type_lower_bound(_) as tlb,a') ->
                        if a' = a1
                          then Some (Constraint(tlb,a2))
                          else None
                    | _ -> None
                )
        )
    (* Now merge the enums to get the resulting stream. *)
    |> Enum.concat
    (* And now it should be a set. *)
    |> Constraint_set.of_enum
;;

