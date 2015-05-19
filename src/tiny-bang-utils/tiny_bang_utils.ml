(** This exception should be raised when code has not yet been implemented. *)
exception Not_yet_implemented of string;;

(** This exception should be raised if a defensive runtime check determines that
    an invariant has been violated. *)
exception Invariant_failure of string;;

let rec natural_compare_seq (parts : (unit -> int) list) =
  match parts with
    | [] -> 0
    | h::t ->
        let r = h () in
        if r = 0 then natural_compare_seq t else r
;;
