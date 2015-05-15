open Batteries;;

open Tiny_bang_types;;

val find_compatibility_cases
        : tvar
       -> Constraint_set.t
       -> pattern_type
       -> Pattern_type_set.t
       -> Constraint_set.t option list

val sensible : filtered_type -> Constraint_set.t -> bool
