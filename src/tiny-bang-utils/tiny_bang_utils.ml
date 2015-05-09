exception Not_yet_implemented of string;;

let rec natural_compare_seq (parts : (unit -> int) list) =
  match parts with
    | [] -> 0
    | h::t ->
        let r = h () in
        if r = 0 then natural_compare_seq t else r
;;
  