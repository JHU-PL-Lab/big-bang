open Batteries;;

type ident = Ident of string;;

type label = Label of ident;;

type var = Var of ident;;

type patFilter =
  | EmptyFilter
  | LabelFilter of label * var
  | ConjunctionFilter of var * var;;

type patFilterRule = PatFilterRule of var * patFilter;;

module PatternFilterRuleSet = Set.Make(
  struct
    type t = patFilterRule
    let compare = compare
  end);;

type pattern = Pattern of var * PatternFilterRuleSet.t;;

type expr = Expr of clause list

and clause = Clause of var * redex

and	redex =
  | ValueRedex of value
  | VarRedex of var
  | ApplRedex of var * var

and	value =
  | EmptyOnionValue
  | LabelValue of label * var
  | OnionValue of var * var
  | FunctionValue of pattern * expr;;
