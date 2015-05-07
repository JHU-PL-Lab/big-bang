open Batteries;;

(* TODO: make use of this structure *)
type srcloc = { srcloc_filename : string
              ; srcloc_lineno : int
              ; srcloc_colno: int
              };;

type ident = Ident of string;;

type label = Label of ident;;

type var = Var of ident;;

type pat_filter =
  | Empty_filter
  | Label_filter of label * var
  | Conjunction_filter of var * var;;

type pat_filter_rule = Pattern_filter_rule of var * pat_filter;;

module Pattern_filter_rule_set = Set.Make(
  struct
    type t = pat_filter_rule
    let compare = compare
  end);;

type pattern = Pattern of var * Pattern_filter_rule_set.t;;

type expr = Expr of clause list

and clause = Clause of var * redex

and	redex =
  | Value_redex of value
  | Var_redex of var
  | Appl_redex of var * var

and	value =
  | Empty_onion_value
  | Label_value of label * var
  | Onion_value of var * var
  | Function_value of pattern * expr;;
