%{
open Tiny_bang_ast;;
open Tiny_bang_parser_support;;
%}
  
%token <string> IDENTIFIER
%token <string> LABEL
%token <int> INT
%token OPEN_BRACE
%token CLOSE_BRACE
%token BACKSLASH
%token SEMICOLON
%token EQUALS
%token AMPERSAND
%token ARROW
%token EMPTY_ONION
%token ASTERISK
%token DOUBLE_SEMICOLON
%token EOF
%token INT_PLUS
%token INT_MINUS
%token INT_TIMES
%token INT_EQUAL
%token INT_LESSTHAN
%token ARRAY_NEW
%token ARRAY_LENGTH
%token ARRAY_GET
%token ARRAY_SET
%token ARRAY
%token REFERENCE_ASSIGN
%token KEYWORD_INT
%token KEYWORD_REF
%token COLON


%start <Tiny_bang_ast.expr> prog
%start <Tiny_bang_ast.expr option> delim_expr

%%

prog:
  | expr EOF
      { $1 }
  ;
  
delim_expr:
  | EOF
      { None }
  | expr DOUBLE_SEMICOLON
      { Some($1) }
  | expr EOF
      { Some($1) }
  ;

expr:
  | separated_nonempty_trailing_list(SEMICOLON, clause)
      { Expr((next_uid $startpos $endpos),$1) }
  ;

clause:
  | variable EQUALS redex
      { Clause((next_uid $startpos $endpos),$1,$3) }
  ;

variable:
  | identifier
      { Var((next_uid $startpos $endpos),$1,None) }
  ;

pattern_variable:
  | identifier
      { Pvar((next_uid $startpos $endpos),$1) }
  ;

label:
  | LABEL
      { Label (Ident $1) }
  ;

identifier:
  | IDENTIFIER
      { Ident $1 }
  ;

builtin:
  | INT_PLUS
      { Op_int_plus }
  | INT_MINUS
      { Op_int_minus }
  | INT_TIMES
      { Op_int_times }
  | INT_EQUAL
      { Op_int_equal }
  | INT_LESSTHAN
      { Op_int_lessthan }
  | ARRAY_NEW
    { Op_array_new }
  | ARRAY_LENGTH
    { Op_array_length }
  | ARRAY_GET
    { Op_array_get }
  | ARRAY_SET
    { Op_array_set }
  | REFERENCE_ASSIGN
      { Op_ref }
  ;

redex:
  | value
      { Value_redex((next_uid $startpos $endpos),$1) }
  | variable
      { Var_redex((next_uid $startpos $endpos),$1) }
  | variable variable
      { Appl_redex((next_uid $startpos $endpos),$1,$2) }
  | builtin list(variable)
      { Builtin_redex((next_uid $startpos $endpos),$1,$2)}
  ;

value:
  | EMPTY_ONION
      { Empty_onion_value(next_uid $startpos $endpos) }
  | INT
      { Int_value((next_uid $startpos $endpos),$1)}
  | label variable
      { Label_value((next_uid $startpos $endpos),$1,$2) }
  | variable AMPERSAND variable
      { Onion_value((next_uid $startpos $endpos),$1,$3) }
  | pattern ARROW OPEN_BRACE expr CLOSE_BRACE
      { Function_value((next_uid $startpos $endpos),$1,$4) }
  | KEYWORD_REF variable
      { Ref_value((next_uid $startpos $endpos),$2)}

pattern:
  | pattern_variable BACKSLASH OPEN_BRACE filter_rule_set CLOSE_BRACE
      { Pattern((next_uid $startpos $endpos),$1,$4) }

filter_rule_set:
  | separated_nonempty_trailing_list(SEMICOLON, filter_rule)
      { 
        let rules = $1 in
        (* Confirm that there are no duplicate variables. *)
        ignore
          (List.fold_left
            (fun s -> fun (x,_) ->
              if Pvar_set.mem x s
                then raise (Tiny_bang_utils.Not_yet_implemented
                              "Duplicate filter rule variable")
                else Pvar_set.add x s
            )
            Pvar_set.empty
            rules);
        Pvar_map.of_enum (BatList.enum $1)
      }

filter_rule:
  | pattern_variable EQUALS filter
      { ($1,$3) }

filter:
  | EMPTY_ONION
      { Empty_filter(next_uid $startpos $endpos) }
  | pattern_variable COLON ARRAY
      { Array_filter((next_uid $startpos $endpos),$1) }
  | pattern_variable COLON KEYWORD_INT
      { Int_filter((next_uid $startpos $endpos),$1)}
  | label pattern_variable
      { Label_filter((next_uid $startpos $endpos),$1,$2) }
  | pattern_variable ASTERISK pattern_variable
      { Conjunction_filter((next_uid $startpos $endpos),$1,$3) }
  | KEYWORD_REF pattern_variable
      { Ref_filter((next_uid $startpos $endpos),$2)}

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
