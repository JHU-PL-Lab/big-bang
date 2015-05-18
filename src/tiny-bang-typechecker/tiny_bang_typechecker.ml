(**
  This module represents the entry point for the TinyBang typechecking process.
*)

open Batteries;;

open Tiny_bang_constraint_closure;;
open Tiny_bang_contours;;
open Tiny_bang_initial_alignment;;
open Tiny_bang_types;;

(**
  Performs typechecking of the provided expression.
  @param e The expression to typecheck.
  @return [true] if the expression typechecks; [false] if it does not.
*)
(* TODO: provide some information to explain the type errors and allow
         constraints to escape. *)
let typecheck e =
  (* Step 1: Initially align the expression. *)
  let (_,cs) = initial_align_expr e in
  (* Step 2: Initial alignment as implemented here does not give the initial
     contour to top-level variables.  We can do this by polyinstantiating the
     expression now. *)
  let bound_vars = Constraint_database.bound_variables_of cs in
  let repl_fn (Tvar(i,_) as a) =
    if Tvar_set.mem a bound_vars then Tvar(i, Some initial_contour) else a
  in
  let cs' = Constraint_database.replace_variables repl_fn cs in
  (* Step 3: Perform constraint closure. *)
  let cs'' = perform_closure cs' in
  (* Step 4: Look for inconsistencies. *)
  let inconsistencies = cs''
    |> Constraint_database.enum
    |> Enum.exists
          (fun c ->
            match c with
              | Inconsistency_constraint -> true
              | _ -> false)
  in
  not inconsistencies
  ;;
