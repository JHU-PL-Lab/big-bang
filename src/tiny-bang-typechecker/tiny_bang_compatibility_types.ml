(** A module containing the types publicly used by the type compatibility
    proof search module. *)

open Batteries;;

open Tiny_bang_types;;

type compatibility_bindings = (filtered_type * tvar) list;;

type compatibility_result =
  | Compatibility_result of compatibility_bindings * bool list
;;

module Compatibility_result_ord =
struct
  type t = compatibility_result
  let compare = compare
end;;

module Compatibility_result_set = Set.Make(Compatibility_result_ord);;
