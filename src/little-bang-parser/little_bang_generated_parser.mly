%{
open Little_bang_ast;;
open Tiny_bang_ast;;
open Tiny_bang_ast_uid;;
open Tiny_bang_parser_support;;
open Tiny_bang_source_origin;;
open Lexing;;

(* FIXME: Refactor the repetition between this and
          Tiny_bang_generated_parser.
*)
let next_uid startpos endpos =
  let uid : ast_uid = next_uid () in
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
  Ast_uid_hashtbl.add (get_ast_position_hash()) uid region;
  uid
%}

%token <string> IDENTIFIER
%token <string> LABEL
%token EQUALS
%token AMPERSAND
%token ARROW
%token EMPTY_ONION
%token LEFT_PAREN
%token RIGHT_PAREN
%token ASTERISK
%token KEYWORD_FUN
%token KEYWORD_LET
%token KEYWORD_IN
%token DOUBLE_SEMICOLON
%token EOF

%left LAM
%right KEYWORD_IN
(* %nonassoc '<=' '>=' '==' *)
(* %right '<-' *)
(* %left '+' '-' *)
%left ASTERISK (* '/' '%' *)
%left AMPERSAND
(* %right 'putChar' *)

%start <Little_bang_ast.expr> prog
%start <Little_bang_ast.expr option> delim_expr

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
  | KEYWORD_FUN pattern ARROW expr %prec LAM
      {
        Value_expr(
          (next_uid $startpos $endpos),
          Function((next_uid $startpos $endpos),$2,$4)
        )
      }
  | expr AMPERSAND expr
      { Onion_expr((next_uid $startpos $endpos),$1,$3) }
  | KEYWORD_LET variable EQUALS expr KEYWORD_IN expr
      { Let_expr((next_uid $startpos $endpos),$2,$4,$6) }
  | appl_expr
      { $1 }
  ;

appl_expr:
  | appl_expr prefix_expr
      { Appl_expr((next_uid $startpos $endpos),$1,$2) }
  | prefix_expr
      { $1 }
  ;

prefix_expr:
  | label prefix_expr
      { Label_expr((next_uid $startpos $endpos),$1,$2) }
  | primary_expr
      { $1 }
  ;

primary_expr:
  | literal
      { Value_expr((next_uid $startpos $endpos),$1) }
  | variable
      { Var_expr((next_uid $startpos $endpos),$1) }
  | LEFT_PAREN expr RIGHT_PAREN
      { $2 }
  ;

literal:
  | EMPTY_ONION
      { Empty_onion((next_uid $startpos $endpos)) }
;

variable:
  | identifier
      { Little_bang_ast.Var((next_uid $startpos $endpos),$1) }
  ;

label:
  | LABEL
      { Label (Ident $1) }
  ;

identifier:
  | IDENTIFIER
      { Ident $1 }
  ;


pattern:
  | pattern ASTERISK pattern
      { Conjunction_pattern((next_uid $startpos $endpos),$1,$3) }
  | primary_pattern
      { $1 }
  ;

primary_pattern:
  | variable
      { Var_pattern((next_uid $startpos $endpos),$1) }
  | EMPTY_ONION
      { Empty_pattern((next_uid $startpos $endpos)) }
  | label primary_pattern
      { Label_pattern((next_uid $startpos $endpos),$1,$2) }
  | LEFT_PAREN pattern RIGHT_PAREN
      { $2 }
  ;
