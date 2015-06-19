open Batteries;;

open Tiny_bang_ast;;

let pretty_ident ident =
  match ident with
  | Ident s -> s
  | Fresh_ident id -> "__" ^ string_of_int id
;;

let pretty_label (Label i) = "`" ^ pretty_ident i;;

let pretty_freshening_stack (Freshening_stack ids) =
  List.fold_left
    (fun acc -> fun i ->
          (* Since freshening stacks are stored in reverse, we reverse the string    *)
          (* here.                                                                   *)
              acc ^ "__" ^ pretty_ident i) "" ids
;;

let pretty_var (Var(_, i, mfs)) =
  match mfs with
  | None -> pretty_ident i
  | Some fs -> pretty_ident i ^ pretty_freshening_stack fs
;;

let pretty_pat_filter pf =
  match pf with
  | Empty_filter(_) -> "()"
  | Label_filter(_, l, x) -> pretty_label l ^ " " ^ pretty_var x
  | Conjunction_filter(_, x1, x2) -> pretty_var x1 ^ " * " ^ pretty_var x2
;;

let pretty_pat_filter_rules (pfrs : pattern_filter_rules) =
  let inner = 
    pfrs
    |> Var_map.enum
    |> Enum.map (fun (x,pf) -> pretty_var x ^ " = " ^ pretty_pat_filter pf)
    |> Enum.fold
        (fun acc -> fun s -> if acc = "" then s else acc ^ "; " ^ s)
        ""
  in
  "{ " ^ inner ^ " }"
;;

let pretty_pattern (Pattern(_, x, pfrs)) =
  pretty_var x ^ " \ " ^ pretty_pat_filter_rules pfrs
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

and pretty_value v =
  match v with
  | Empty_onion_value(_) -> "()"
  | Label_value(_, l, x) -> pretty_label l ^ " " ^ pretty_var x
  | Onion_value(_, x1, x2) -> pretty_var x1 ^ " & " ^ pretty_var x2
  | Function_value(_, p, e) -> pretty_pattern p ^ " -> " ^ pretty_expr e
;;

