%{
open TinyBang_Ast;;
%}
  
%token <string> IDENTIFIER
%token <string> LABEL
%token OPEN_BRACE
%token CLOSE_BRACE
%token BACKSLASH
%token SEMICOLON
%token EQUALS
%token AMPERSAND
%token ARROW
%token EMPTY_ONION
%token ASTERISK
%token EOF

%start <TinyBang_Ast.expr> prog

%%

prog:
  | expr EOF
      { $1 }
  ;

expr:
  | separated_nonempty_list(SEMICOLON, clause) SEMICOLON?
      { Expr $1 }
  ;

clause:
  | variable EQUALS redex
      { Clause($1,$3) }
  ;

variable:
  | identifier
      { Var $1 }
  ;
  
label:
  | LABEL
      { Label (Ident $1) }
  ;
  
identifier:
  | IDENTIFIER
      { Ident $1 }
  ;

redex:
  | value
      { ValueRedex $1 }
  | variable
      { VarRedex $1 }
  | variable variable
      { ApplRedex($1,$2) }
  ;

value:
  | EMPTY_ONION
      { EmptyOnionValue }
  | label variable
      { LabelValue($1,$2) }
  | variable AMPERSAND variable
      { OnionValue($1,$3) }
  | pattern ARROW OPEN_BRACE expr CLOSE_BRACE
      { FunctionValue($1,$4) }

pattern:
  | variable BACKSLASH OPEN_BRACE filterRuleSet CLOSE_BRACE
      { Pattern($1,$4) }

filterRuleSet:
  | separated_nonempty_list(SEMICOLON, filterRule) SEMICOLON?
      { PatternFilterRuleSet.of_list $1 }

filterRule:
  | variable EQUALS filter
      { PatFilterRule($1,$3) }

filter:
  | EMPTY_ONION
      { EmptyFilter }
  | label variable
      { LabelFilter($1,$2) }
  | variable ASTERISK variable
      { ConjunctionFilter($1,$3) }


