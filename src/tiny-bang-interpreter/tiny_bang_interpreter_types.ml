open Batteries;;

module EnvironmentHash =
struct
  type t = Tiny_bang_ast.var
  let equal = Tiny_bang_ast.var_equal
  let hash = Tiny_bang_ast.var_hash
end;;

module Environment = Hashtbl.Make(EnvironmentHash);;