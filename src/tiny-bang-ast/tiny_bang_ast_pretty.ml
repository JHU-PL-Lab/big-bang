open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_string_utils;;

let pretty_builtin_op op =
  match op with
  | Op_int_plus  -> "int+"
  | Op_int_minus -> "int-"
  | Op_int_times -> "int*"
  | Op_int_equal -> "int="
  | Op_int_lessthan -> "int<"
  | Op_ref -> ":="
  | Op_array_new -> "arrayNew"
  | Op_array_length -> "arrayLength"
  | Op_array_get -> "arrayGet"
  | Op_array_set -> "arraySet"
;;

let rec pretty_ident ident =
  match ident with
  | Ident s -> s
  | Fresh_ident id -> "__" ^ string_of_int id
  | Builtin_ident(op,n) -> "__" ^ pretty_builtin_op op ^ "_" ^ string_of_int n
  | Builtin_local_ident(op,basis,n) -> "__" ^ pretty_builtin_op op ^ "__based_on__" ^ (pretty_ident basis) ^ "__" ^ string_of_int n
;;

let pretty_label (Label i) = "`" ^ pretty_ident i;;

let pretty_freshening_stack (Freshening_stack ids) =
  List.fold_left
    (fun acc -> fun i ->
       (* Since freshening stacks are stored in reverse, we reverse the string    *)
       (* here.                                                                   *)
       acc ^ "__" ^ pretty_ident i) "" ids
;;

let pretty_pvar (Pvar(_,i)) = pretty_ident i;;

let pretty_var (Var(_, i, mfs)) =
  match mfs with
  | None -> pretty_ident i
  | Some fs -> pretty_ident i ^ pretty_freshening_stack fs
;;

let pretty_pat_filter pf =
  match pf with
  | Empty_filter(_) -> "()"
  | Label_filter(_, l, x) -> pretty_label l ^ " " ^ pretty_pvar x
  | Conjunction_filter(_, x1, x2) -> pretty_pvar x1 ^ " * " ^ pretty_pvar x2
  | Int_filter(_, x1) -> pretty_pvar x1 ^ ": int "
  | Ref_filter(_, x1) -> "ref " ^ pretty_pvar x1
  | Array_filter(_) -> "array"
;;

let pretty_pat_filter_rules (pfrs : pattern_filter_rules) =
  let inner = 
    pfrs
    |> Pvar_map.enum
    |> Enum.map (fun (x,pf) -> pretty_pvar x ^ " = " ^ pretty_pat_filter pf)
    |> concat_sep "; "
  in
  "{ " ^ inner ^ " }"
;;

let pretty_pattern (Pattern(_, x, pfrs)) =
  pretty_pvar x ^ " \ " ^ pretty_pat_filter_rules pfrs
;;

let rec pretty_expr (Expr(_, cls)) =
  List.fold_left
    (fun acc -> fun s -> if acc = "" then s else acc ^ "; " ^ s)
    ""
    (List.map pretty_clause cls)

and pretty_clause (Clause(_, x, r)) =
  pretty_var x ^ " = " ^ pretty_redex r

and pretty_redex r =
  match r with
  | Value_redex(_, v) -> pretty_value v
  | Var_redex(_, x) -> pretty_var x
  | Appl_redex(_, x1, x2) -> pretty_var x1 ^ " " ^ pretty_var x2
  | Builtin_redex(_, o, lv) ->
    pretty_builtin_op o ^ " " ^
    concat_sep " " (Enum.map pretty_var @@ List.enum lv)

and pretty_value v =
  match v with
  | Empty_onion_value(_) -> "()"
  | Int_value(_,x) -> string_of_int x
  | Array_value(_,x) -> Tiny_bang_string_utils.concat_sep_delim "array {" "}" ", " @@
    Enum.map pretty_var @@ BatArray.enum x
  | Label_value(_, l, x) -> pretty_label l ^ " " ^ pretty_var x
  | Onion_value(_, x1, x2) -> pretty_var x1 ^ " & " ^ pretty_var x2
  | Function_value(_, p, e) -> pretty_pattern p ^ " -> " ^ pretty_expr e
  | Ref_value(_,x) -> "ref " ^ pretty_var x
;;

