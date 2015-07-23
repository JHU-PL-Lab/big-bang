open Tiny_bang_compatibility_types;;
open Tiny_bang_types;;

val find_compatibility_cases
  : tvar
  -> Constraint_database.t
  -> pattern_type
  -> Pattern_type_set.t
  -> compatibility_bindings option list

val sensible : filtered_type -> Constraint_database.t -> bool
