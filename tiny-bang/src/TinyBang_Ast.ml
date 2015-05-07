module TinyBang_Ast =
struct
(*	
	type expr = Expr of clause list;;
	
	type clause = Clause of var * redex;;
	
	type redex = ValueRedex of value | VarRedex of var | ApplRedex of var * var;;
	
	type var = Var of ident;;
	
	type ident = Ident of string;;

type value = EmptyOnionValue | LabelValue of label * var | OnionValue of var * var | FunctionValue of pattern * expr;;

type pattern = Pattern of var * patfilter
	*)
end;;