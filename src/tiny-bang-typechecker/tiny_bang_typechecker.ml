(**
  This module represents the entry point for the TinyBang typechecking process.
*)

open Batteries;;
open Printf;;

open Tiny_bang_ast_pretty;;
open Tiny_bang_constraint_closure;;
open Tiny_bang_contours;;
open Tiny_bang_initial_alignment;;
open Tiny_bang_types;;
open Tiny_bang_types_pretty;;

(* ************************************************************************** *)
(* LOGGER *)

let logger = Tiny_bang_logger.make_logger "Tiny_bang_typechecker";;

(* ************************************************************************** *)
(* TYPECHECKING *)

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
  logger `trace
    (sprintf
      "Initial alignment of %s yields constraints %s"
      (pretty_expr e) (pretty_constraints cs)
    )
  ;
  (* Step 2: Initial alignment as implemented here does not give the initial
     contour to top-level variables.  We can do this by polyinstantiating the
     expression now. *)
  let bound_vars = Constraint_database.bound_variables_of cs in
  let repl_fn (Tvar(i,_) as a) =
    if Tvar_set.mem a bound_vars then Tvar(i, Some initial_contour) else a
  in
  let cs' = Constraint_database.replace_variables repl_fn cs in
  logger `trace
    (sprintf
      "Initial contour instantiation yields constraints %s"
      (pretty_constraints cs')
    )
  ;
  (* Step 3: Perform constraint closure. *)
  let cs'' = perform_closure cs' in
  logger `trace
    (sprintf
      "Constraint closure yields constraints %s"
      (pretty_constraints cs'')
    )
  ;
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
