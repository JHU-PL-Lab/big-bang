open Batteries;;

open Tiny_bang_types;;

type application_matching_result =
  | Application_matching_result of
      (tvar * Constraint_database.t * Constraint_database.t) option
;;

module Application_matching_result_ord =
struct
  type t = application_matching_result
  let compare = compare
end;;

module Application_matching_result_set =
  Set.Make(Application_matching_result_ord)
;;
