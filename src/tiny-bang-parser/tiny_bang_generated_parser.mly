%{
open Tiny_bang_ast;;
open Tiny_bang_parser_support;;
open Tiny_bang_parser_types;;
open Lexing;;

let next_uid startpos endpos =
  let uid : ast_uid = Tiny_bang_ast.next_uid () in
  let start = { file_pos_lineno = startpos.pos_lnum
              ; file_pos_colno = startpos.pos_bol
              } in
  let stop = { file_pos_lineno = endpos.pos_lnum
             ; file_pos_colno = endpos.pos_bol
             } in
  let region = { file_region_filename = startpos.pos_fname
               ; file_region_start = start
               ; file_region_end = stop
               } in
  Ast_uid_hashtbl.add !ast_position_hash uid region;
  uid
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
%token DOUBLE_SEMICOLON
%token EOF

%start <Tiny_bang_ast.expr> prog
%start <Tiny_bang_ast.expr * bool> delim_expr

%%

prog:
  | expr EOF
      { $1 }
  ;
  
delim_expr:
  | expr EOF
      { ($1,true) }
  | expr DOUBLE_SEMICOLON
      { ($1,false) }
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
      { Var((next_uid $startpos $endpos),$1,Freshening_stack None) }
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
      { Value_redex((next_uid $startpos $endpos),$1) }
  | variable
      { Var_redex((next_uid $startpos $endpos),$1) }
  | variable variable
      { Appl_redex((next_uid $startpos $endpos),$1,$2) }
  ;

value:
  | EMPTY_ONION
      { Empty_onion_value(next_uid $startpos $endpos) }
  | label variable
      { Label_value((next_uid $startpos $endpos),$1,$2) }
  | variable AMPERSAND variable
      { Onion_value((next_uid $startpos $endpos),$1,$3) }
  | pattern ARROW OPEN_BRACE expr CLOSE_BRACE
      { Function_value((next_uid $startpos $endpos),$1,$4) }

pattern:
  | variable BACKSLASH OPEN_BRACE filter_rule_set CLOSE_BRACE
      { Pattern((next_uid $startpos $endpos),$1,$4) }

filter_rule_set:
  | separated_nonempty_trailing_list(SEMICOLON, filter_rule)
      { Pattern_filter_rule_set.of_list $1 }

filter_rule:
  | variable EQUALS filter
      { Pattern_filter_rule((next_uid $startpos $endpos),$1,$3) }

filter:
  | EMPTY_ONION
      { Empty_filter(next_uid $startpos $endpos) }
  | label variable
      { Label_filter((next_uid $startpos $endpos),$1,$2) }
  | variable ASTERISK variable
      { Conjunction_filter((next_uid $startpos $endpos),$1,$3) }

separated_nonempty_trailing_list(separator, rule):
  | nonempty_list(terminated(rule, separator))
      { $1 }
  | separated_nonempty_list(separator,rule)
      { $1 }
