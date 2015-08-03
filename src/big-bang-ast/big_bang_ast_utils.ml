open Batteries;;

open Tiny_bang_utils;;

let new_fresh_identifier () =
  let new_fresh_identifier_id =
    match Little_bang_ast.new_fresh_ident () with
    | Little_bang_ast.Fresh_ident repeat_identifier_id -> repeat_identifier_id
    | _ -> raise @@ Invariant_failure "`Little_bang_ast.new_fresh_ident' returned something other than an `Little_bang_ast.Fresh_ident'."
  in
  Big_bang_ast.Fresh_identifier new_fresh_identifier_id
;;
