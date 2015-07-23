%{
(* open Big_bang_ast;; *)
(* open Little_bang_ast;; *)
(* open Tiny_bang_ast;; *)
(* open Tiny_bang_ast_uid;; *)
(* open Tiny_bang_parser_support;; *)
(* open Tiny_bang_source_origin;; *)
(* open Lexing;; *)


(* FIXME: Refactor the repetition between this, Little_bang_generated_parser and
          Tiny_bang_generated_parser.
*)
(*
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
;;
*)
%}

(**********)
(* Tokens *)
(**********)

(* Symbols. *)

%token EQUALS
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token AMPERSAND
%token DOT
%token COMMA
%token SEMICOLON
%token UNDERSCORE
%token DOUBLE_SEMICOLON

(* Grouping symbols. *)

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token LEFT_SQUARE_BRACKET
%token RIGHT_SQUARE_BRACKET
%token LEFT_CURLY_BRACKET
%token RIGHT_CURLY_BRACKET

(* Operators. *)

%token OPERATOR_EQUALITY
%token OPERATOR_INEQUALITY

(* Keywords. *)

%token TRUE
%token FALSE
%token AND
%token OR
%token XOR
%token NOT
%token FUN
%token DEF
%token LET
%token REF
%token IF
%token THEN
%token ELSE_IF
%token ELSE
%token END
%token MATCH
%token AS
%token IN
%token REPEAT
%token WHILE
%token DO
%token RETURN
%token OBJECT
%token INCLUDE
%token PUBLIC
%token PRIVATE

(* Literals. *)

%token          EMPTY_ONION
%token <int>    INTEGER
%token <char>   CHARACTER
%token <string> TEXT
%token <string> LABEL

(* Identifier. *)

%token <string> IDENTIFIER

(* Delimiters. *)

%token NEWLINE
%token EOF

(***************************)
(* Precedence declarations *)
(***************************)

%left     OR
%left     AND
%nonassoc XOR
%nonassoc UNARY_PREFIX_OPERATION_PRECEDENCE
%nonassoc OPERATOR_EQUALITY OPERATOR_INEQUALITY
%left     PLUS MINUS
%left     ASTERISK SLASH
%left     AMPERSAND

(*************************)
(* Grammar start symbols *)
(*************************)

(* %start <Big_bang_ast.program> source_file_program *)
(* %start <Big_bang_ast.program option> toploop_program *)
%start <unit> source_file_program
%start <unit> toploop_program

%%

(*****************)
(* Grammar rules *)
(*****************)

(* Hints for reading grammar rules:

   1. `__' consumes any amount of newlines (possibly zero). It means newlines
      are ignored at that point.
   2. `__' should be used inside rules to separate its producers. Using `__'
      on the boundaries (start or end) of rules should be avoided except in
      special cases (e.g. dividers).
   3. Rules marked as `%public' should be read first in order to understand how
      to write Big Bang code. Rules marked as `%inline' are of minor interest.
   4. `%prec' and `%inline' can't be mixed together. So `%inline' is left as a
      comment on some rules for documentation purposes.
   5. The grammar is divided in sections, separated by bars made of comments.
   6. Names that end in `_' are reserved keywords in OCaml and thus aren't legal
      identifiers. This is a convention borrowed from Python since OCaml code
      style doesn't mention the case.
   7. Tokens that contain values (e.g. TEXT, LABEL) have their own rules.
   8. Rules are ordered from top (most general) to bottom (most specific).
*)

(******************************************************************************)

%public
source_file_program:
  | __ option(block_of(top_level_block_expression)) EOF
    { () }
  ;

%public
toploop_program:
  | __ option(block_of(top_level_block_expression)) toploop_program_terminator
    { () }
  ;

%inline
toploop_program_terminator:
  | DOUBLE_SEMICOLON
    { () }
  | EOF
    { () }
  ;

(******************************************************************************)

(* There are two kinds of expressions. Those that show up in blocks (e.g. the
   top level or function and method bodies), and those that are inlined (e.g.
   values of a record).

   The ones that show up in blocks don't take a `block_expression' as
   parameters. They are supposed to be used as
   `block_of(<type>_block_expression)'.

   The ones that are inlined need to know what kind of blocks they
   are in, so they take `block_expression' as parameter. *)

(* Top level expressions are those that appear out of functions or methods. *)
%public
top_level_block_expression:
  | literal(top_level_block_expression)
    { () }
  | operation(top_level_block_expression)
    { () }
  | selection(top_level_block_expression)
    { () }
  | application_or_indexing(top_level_block_expression)
    { () }
  | method_call(top_level_block_expression)
    { () }
  | identifier
    { () }
  | assignment(top_level_block_expression)
    { () }
  | flow_control(top_level_block_expression)
    { () }
  | function_abstraction
    { () }
  | LEFT_PARENTHESIS __ block_of(top_level_block_expression) RIGHT_PARENTHESIS
    { () }
  ;

%public
function_or_method_body_block_expression:
  | literal(function_or_method_body_block_expression)
    { () }
  | operation(function_or_method_body_block_expression)
    { () }
  | selection(function_or_method_body_block_expression)
    { () }
  | application_or_indexing(function_or_method_body_block_expression)
    { () }
  | method_call(function_or_method_body_block_expression)
    { () }
  | identifier
    { () }
  | assignment(function_or_method_body_block_expression)
    { () }
  | flow_control(function_or_method_body_block_expression)
    { () }
  | function_abstraction
    { () }
  | return(function_or_method_body_block_expression)
    { () }
  | LEFT_PARENTHESIS __ block_of(function_or_method_body_block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* `value_block_expression' was designed for the `include' section of objects
   where a list (block) of values is required.

   IMPORTANT: It should be kept in sync with `value_inline_expression'! *)
%public
value_block_expression:
  | literal(value_block_expression)
    { () }
  | operation(value_block_expression)
    { () }
  | selection(value_block_expression)
    { () }
  | application_or_indexing(value_block_expression)
    { () }
  | method_call(value_block_expression)
    { () }
  | identifier
    { () }
  | flow_control(value_block_expression)
    { () }
  | LEFT_PARENTHESIS __ block_of(value_block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* `value_inline_expression' can be used where a value is expected. For example, on
   label values, record elements and as actual parameters.

   IMPORTANT: It should be kept in sync with `value_block_expression'! *)
%public
value_inline_expression(block_expression):
  | literal(block_expression)
    { () }
  | operation(block_expression)
    { () }
  | selection(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | method_call(block_expression)
    { () }
  | identifier
    { () }
  | flow_control(block_expression)
    { () }
  | LEFT_PARENTHESIS __ block_of(block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* Conditional expressions are those which may appear in the condition part of
   e.g. an if or while expression. *)
%public
condition_inline_expression(block_expression):
  | literal(block_expression)
    { () }
  | operation(block_expression)
    { () }
  | selection(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | method_call(block_expression)
    { () }
  | identifier
    { () }
  | LEFT_PARENTHESIS __ block_of(block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* Selectable expressions are those that can show up in selections (to the left
   of the `.'). *)
%public
selectable_inline_expression(block_expression):
  | delimited_literal(block_expression)
    { () }
  | selection(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | method_call(block_expression)
    { () }
  | identifier
    { () }
  | LEFT_PARENTHESIS __ block_of(block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* Assignable expressions are those that can show up in mutable update
   assignments (to the left of the `='). *)
%public
assignable_inline_expression(block_expression):
  | selection(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | identifier
    { () }
  ;

(* Applicable or indexable expressions are those that can show up in function
   application and indexing and method calls (right before the parenthesis with
   the actual parameters or indexes).

   This expression rule, despite being inline, doesn't consume the newlines
   following it. The reason is that newlines aren't allowed between the function
   being applied and its actual arguments because that would be ambiguous. For
   example, let `f' be a function and `x' its argument, the following code would
   be ambiguous:

     f
     (x)

   It could mean either a function call or a block in which the first expression
   is the bare function `f' and the second is the value `x' surrounded by
   parenthesis.
*)
%public
applicable_or_indexable_inline_expression(block_expression):
  | delimited_literal(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | method_call(block_expression)
    { () }
  | identifier
    { () }
  | flow_control(block_expression)
    { () }
  | LEFT_PARENTHESIS __ block_of(block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(* Labelable expressions are those that can be labeled (after the label itself). *)
%public
labelable_inline_expression(block_expression):
  | literal(block_expression)
    { () }
  | unary_prefix_operation(block_expression)
    { () }
  | selection(block_expression)
    { () }
  | application_or_indexing(block_expression)
    { () }
  | method_call(block_expression)
    { () }
  | identifier
    { () }
  | flow_control(block_expression)
    { () }
  | LEFT_PARENTHESIS __ block_of(block_expression) RIGHT_PARENTHESIS
    { () }
  ;

(******************************************************************************)

(* Pattern rules are named after their expression correlates with the `_pattern'
  suffix. *)

%public
pattern:
  | literal_pattern
    { () }
  | operation_pattern
    { () }
  | identifier_pattern
    { () }
  | LEFT_PARENTHESIS __ pattern __ RIGHT_PARENTHESIS
    { () }
  ;

(* Labelable patterns are those that can be labeled (after the label itself). *)
%public
labelable_pattern:
  | literal_pattern
    { () }
  | identifier_pattern
    { () }
  | LEFT_PARENTHESIS __ pattern __ RIGHT_PARENTHESIS
    { () }
  ;

%inline
literal_pattern:
  | primitive_literal_pattern
    { () }
  | structured_literal_pattern
    { () }
  ;

%inline
primitive_literal_pattern:
  | primitive_literal
    { () }
  ;

%inline
structured_literal_pattern:
  | delimited_structured_literal_pattern
    { () }
  | open_ended_structured_literal_pattern
    { () }
  ;

%inline
delimited_structured_literal_pattern:
  | record_pattern
    { () }
  | list_pattern
    { () }
  ;

%inline
open_ended_structured_literal_pattern:
  | prefix_structured_literal_pattern
    { () }
  ;

%inline
prefix_structured_literal_pattern:
  | labeled_data_pattern
    { () }
  ;

%inline
record_pattern:
  | LEFT_CURLY_BRACKET __
      option(list_of(record_field_pattern, list_divider))
    RIGHT_CURLY_BRACKET
    { () }
  ;

%inline
record_field_pattern:
  | identifier __ EQUALS __ pattern
    { () }
  ;

%inline
list_pattern:
  | LEFT_SQUARE_BRACKET __
      option(list_of(list_field_pattern, list_divider))
      option(terminated(list_field_remainder_pattern, __))
    RIGHT_SQUARE_BRACKET
    { () }
  ;

%inline
list_field_pattern:
  | pattern
    { () }
  ;

%inline
list_field_remainder_pattern:
  | ASTERISK (* NO NEWLINE *) identifier_pattern
    { () }
  ;

%inline
labeled_data_pattern:
  | label __ labelable_pattern
    { () }
  ;

%inline
operation_pattern:
  | binary_operation_pattern
    { () }
  ;

%inline
binary_operation_pattern:
  | pattern (* NO NEWLINE *) binary_operator_pattern __ pattern
    { () }
  ;

%inline
binary_operator_pattern:
  | conjunction_operator_pattern
    { () }
  ;

%inline
conjunction_operator_pattern:
  | AND
    { () }
  ;

%inline
identifier_pattern:
  | identifier
    { () }
  | catch_all_identifier_pattern
    { () }
  ;

%inline
catch_all_identifier_pattern:
  | UNDERSCORE
    { () }
  ;

(******************************************************************************)

%public
literal(block_expression):
  | primitive_literal
    { () }
  | structured_literal(block_expression)
    { () }
  ;

(* `delimited_literal' are literals that are not open ended (e.g. numbers and
   anonymous functions are delimited (thanks to the `end' keyword), while
   labeled data is open ended).

   The purpose of this rule is to allow for literals to be subjects in contexts
   such as selection and function application, in which open ended literals
   would be ambiguous.

   For example, the following expression:

     `foo not (bar).baz

   Could be interpreted as either:

     `foo not ((bar).baz)

   Or:

     (`foo not (bar)).baz

   We want the first option, not the second, so we disallow for literal labeled
   data to show up as the selection subject.
*)
%public
delimited_literal(block_expression):
  | primitive_literal
    { () }
  | delimited_structured_literal(block_expression)
    { () }
  ;

%inline
primitive_literal:
  | boolean
    { () }
  | number
    { () }
  | character
    { () }
  | text
    { () }
  | empty_onion
    { () }
  ;

%inline
structured_literal(block_expression):
  | delimited_structured_literal(block_expression)
    { () }
  | open_ended_structured_literal(block_expression)
    { () }
  ;

%inline
delimited_structured_literal(block_expression):
  | record(block_expression)
    { () }
  | list_(block_expression)
    { () }
  | anonymous_function
    { () }
  | object_
    { () }
  ;

%inline
open_ended_structured_literal(block_expression):
  | prefix_structured_literal(block_expression)
    { () }
  ;

%inline
prefix_structured_literal(block_expression):
  | labeled_data(block_expression)
    { () }
  ;

%inline
boolean:
  | TRUE
    { () }
  | FALSE
    { () }
 ;

%inline
number:
  | integer
    { () }
  ;

%inline
integer:
  | INTEGER
    { () }
  ;

%inline
character:
  | CHARACTER
    { () }
  ;

%inline
text:
  | TEXT
    { () }
  ;

%inline
empty_onion:
  | EMPTY_ONION
    { () }
  ;

%inline
record(block_expression):
  | LEFT_CURLY_BRACKET __
      option(list_of(record_field(block_expression), list_divider))
    RIGHT_CURLY_BRACKET
    { () }
  ;

%inline
record_field(block_expression):
  | identifier __ EQUALS __ value_inline_expression(block_expression)
    { () }
  ;

(* It's very important that this rule is called `list_' instead of just
`list', as Menhir comes with a helper function called `list' and we don't want
to shadow it. *)
%inline
list_(block_expression):
  | LEFT_SQUARE_BRACKET __
      option(list_of(list_field(block_expression), list_divider))
    RIGHT_SQUARE_BRACKET
    { () }
  ;

%inline
list_field(block_expression):
  | value_inline_expression(block_expression)
    { () }
  ;

%inline
labeled_data(block_expression):
  | label __ labelable_inline_expression(block_expression)
    { () }
  ;

%inline
label:
  | LABEL
    { () }
  ;

(******************************************************************************)

%inline
anonymous_function:
  | FUN __ formal_parameters __ EQUALS __
      option(block_of(function_or_method_body_block_expression))
    END
    { () }
  ;

%inline
formal_parameters:
  | LEFT_PARENTHESIS __
      option(list_of(formal_parameter, list_divider))
    RIGHT_PARENTHESIS
    { () }
  ;

%inline
formal_parameter:
  | identifier
    { () }
  ;

(******************************************************************************)

%inline
object_:
  | OBJECT __
      list(object_section)
    END
    { () }
  ;

%inline
object_section:
  | object_include_section
    { () }
  | object_member_section
    { () }
  ;

%inline
object_include_section:
  | INCLUDE __ object_include_section_body
    { () }
  ;

%inline
object_include_section_body:
  | option(block_of(value_block_expression))
    { () }
  ;

%inline
object_member_section:
  | object_member_section_header __ object_member_section_body
    { () }
  ;

%inline
object_member_section_header:
  | PUBLIC
    { () }
  | PRIVATE
    { () }
  ;

%inline
object_member_section_body:
  | option(block_of(object_member))
    { () }
  ;

%inline
object_member:
  | object_field_definition
    { () }
  | method_definition
    { () }
  ;

%inline
object_field_definition:
  | immutable_assignment(value_block_expression)
    { () }
  | mutable_assignment(value_block_expression)
    { () }
  ;

%inline
method_definition:
  | DEF __ identifier (* NO NEWLINE *) formal_parameters __ EQUALS __
      option(block_of(function_or_method_body_block_expression))
    END
    { () }
  ;

(******************************************************************************)

%public
operation(block_expression):
  | binary_operation(block_expression)
    { () }
  | unary_prefix_operation(block_expression)
    { () }
  ;

%inline
binary_operation(block_expression):
  | value_inline_expression(block_expression) (* NO NEWLINE *)
    binary_operator __
    value_inline_expression(block_expression)
    { () }
  ;

%inline
binary_operator:
  | primitive_binary_operator
    { () }
  | structured_binary_operator
    { () }
  | general_binary_operator
    { () }
  ;

%inline
primitive_binary_operator:
  | boolean_binary_operator
    { () }
  | number_binary_operator
    { () }
  ;

%inline
structured_binary_operator:
  | onioning_operator
    { () }
  ;

%inline
general_binary_operator:
  | comparison_operator
    { () }
  ;

%inline
boolean_binary_operator:
  | AND
    { () }
  | OR
    { () }
  | XOR
    { () }
  ;

%inline
number_binary_operator:
  | PLUS
    { () }
  | MINUS
    { () }
  | ASTERISK
    { () }
  | SLASH
    { () }
  ;

%inline
onioning_operator:
  | AMPERSAND
    { () }
  ;

%inline
comparison_operator:
  | OPERATOR_EQUALITY
    { () }
  | OPERATOR_INEQUALITY
    { () }
  ;

(* %inline *)
unary_prefix_operation(block_expression):
  | unary_prefix_operator __ value_inline_expression(block_expression)
    { () }
    %prec UNARY_PREFIX_OPERATION_PRECEDENCE
  ;

%inline
unary_prefix_operator:
  | primitive_unary_prefix_operator
    { () }
  ;

%inline
primitive_unary_prefix_operator:
  | boolean_unary_prefix_operator
    { () }
  ;

%inline
boolean_unary_prefix_operator:
  | NOT
    { () }
  ;

(******************************************************************************)

(* Selection works on records and objects (for fields, not methods). *)
%public
selection(block_expression):
  | selectable_inline_expression(block_expression) DOT __ identifier
    { () }
  ;

(******************************************************************************)

(* This rule conflates the syntax for function application and indexing
   (an operation) in texts and lists. They are spelled the same. *)
%public
application_or_indexing(block_expression):
  | applicable_or_indexable_inline_expression(block_expression)
    actual_parameters_or_indexes(block_expression)
    { () }
  ;

%public
method_call(block_expression):
  | selectable_inline_expression(block_expression) DOT __
    identifier (* NO NEWLINE *) actual_parameters_or_indexes(block_expression)
    { () }
  ;

%inline
actual_parameters_or_indexes(block_expression):
  | LEFT_PARENTHESIS __
      option(list_of(actual_parameter_or_index(block_expression), list_divider))
    RIGHT_PARENTHESIS
    { () }
  ;

%inline
actual_parameter_or_index(block_expression):
  | value_inline_expression(block_expression)
    { () }
  ;

(******************************************************************************)

%public
identifier:
  | IDENTIFIER
    { () }
  ;

(******************************************************************************)

%public
assignment(block_expression):
  | immutable_assignment(block_expression)
    { () }
  | mutable_assignment(block_expression)
    { () }
  | mutable_update_assignment(block_expression)
    { () }
  ;

%inline
immutable_assignment(block_expression):
  | LET __ unqualified_assignment(block_expression)
    { () }
  ;

%inline
mutable_assignment(block_expression):
  | REF __ unqualified_assignment(block_expression)
    { () }
  ;

%inline
mutable_update_assignment(block_expression):
  | assignable_inline_expression(block_expression) (* NO NEWLINE *) EQUALS __
    value_inline_expression(block_expression)
    { () }
  ;

%inline
unqualified_assignment(block_expression):
  | identifier (* NO NEWLINE *) EQUALS __
    value_inline_expression(block_expression)
    { () }
  ;

(******************************************************************************)

%public
flow_control(block_expression):
  | conditional(block_expression)
    { () }
  | repetition(block_expression)
    { () }
  ;

%inline
conditional(block_expression):
  | if_(block_expression)
    { () }
  | pattern_match(block_expression)
    { () }
  ;

%inline
if_(block_expression):
  | IF __ condition_inline_expression(block_expression) then_
      option(block_of(block_expression))
    list(else_if(block_expression))
    option(else_(block_expression))
    END
    { () }
  ;

%inline
else_if(block_expression):
  | ELSE_IF __ condition_inline_expression(block_expression) then_
      option(block_of(block_expression))
    { () }
  ;

%inline
else_(block_expression):
  | ELSE __
      option(block_of(block_expression))
    { () }
  ;

%inline
pattern_match(block_expression):
  | MATCH __ condition_inline_expression(block_expression) __
    list(pattern_match_case(block_expression))
    END
    { () }
  ;

%inline
pattern_match_case(block_expression):
  | AS __ pattern __ IN __ option(block_of(block_expression))
    { () }
  ;

%inline
repetition(block_expression):
  | REPEAT __ while_(block_expression)
    { () }
  ;

%inline
while_(block_expression):
  | WHILE __ condition_inline_expression(block_expression) do_
      option(block_of(block_expression))
    END
    { () }
  ;

(******************************************************************************)

%public
function_abstraction:
  | FUN __ identifier (* NO NEWLINE *) formal_parameters __ EQUALS __
      option(block_of(function_or_method_body_block_expression))
    END
    { () }
  ;

(******************************************************************************)

%public
return(block_expression):
  | RETURN __ value_inline_expression(block_expression)
    { () }
  ;

(******************************************************************************)

(* Double underscore are used anywhere newlines can be ignored. *)
%public
__:
  | NEWLINE?
    { () }
  ;

%public
block_divider:
  | NEWLINE
    { () }
  | SEMICOLON __
    { () }
  ;

(* `list_divider' is the thing that divides lists such as the list of fields of
   a record, the elements of a list literal or the formal or formal or actual
   parameter lists. *)
%public
list_divider:
  | __ COMMA __
    { () }
  ;

(* `then_' is used to separate the condition from the consequence on the
   `if' expression. *)
%public
then_:
  | NEWLINE
    { () }
  | THEN __
    { () }
  ;

(* `do_' is used to separate the condition from the consequence
   on the `repeat' expressions. *)
%public
do_:
  | NEWLINE
    { () }
  | DO __
    { () }
  ;

(******************************************************************************)

%public
block_of(expression):
  | list_of(expression, block_divider)
    { () }
  ;

(* Non-empty empty list of separated elements with optional trailing separator.

   Examples:

     `'         - Not accepted, empty list.
     `1, 2, 3'  - Accepted, list without trailing separator.
     `1, 2, 3,' - Accepted, list with trailing separator.
     `,'        - Not accepted, only the trailing separator.
*)
%public
list_of(element, separator):
  | nonempty_list(terminated(element, separator))
      { () }
  | separated_nonempty_list(separator, element)
      { () }
  ;

(*********)
(* Tests *)
(*********)

(* To run the tests, run Menhir adding the `--interpret' and
   `--interpret-show-cst' flags.

   What I did to make this happen was to:

   1. Run `make', look for the line that runs `menhir'.
   2. `cd' into the `_build/' directory.
   3. Run the line captured in step 1 while adding the mentioned flags
      right after the `--explain' and `--infer' flags.

   For example: menhir --ocamlc '/Users/leafac/.opam/system/bin/ocamlfind ocamlc -w @A-4-44 -g -annot -bin-annot -I src/tiny-bang-utils -I src/tiny-bang-ast -I src/little-bang-ast -package batteries -package monadlib -I src/big-bang-parser -I src/little-bang-ast -I src/tiny-bang-ast -I src/tiny-bang-utils' --explain --infer --interpret --interpret-show-cst src/big-bang-parser/big_bang_generated_parser.mly

   This makes Menhir run in interpreter mode, instead of generating
   the parser. It lets you interact with the parser and inspect the
   generated parse trees.
*)

(*
Empty program.

source_file_program: EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):]
  EOF
]
*)

(*
Record selection.

source_file_program: IDENTIFIER DOT IDENTIFIER EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [selection(top_level_block_expression):
              [selectable_inline_expression(top_level_block_expression):
                [identifier: IDENTIFIER]
              ]
              DOT
              [__: [option(NEWLINE):]]
              [identifier: IDENTIFIER]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Two record selections in a row.

source_file_program: IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [selection(top_level_block_expression):
              [selectable_inline_expression(top_level_block_expression):
                [selection(top_level_block_expression):
                  [selectable_inline_expression(top_level_block_expression):
                    [identifier: IDENTIFIER]
                  ]
                  DOT
                  [__: [option(NEWLINE):]]
                  [identifier: IDENTIFIER]
                ]
              ]
              DOT
              [__: [option(NEWLINE):]]
              [identifier: IDENTIFIER]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Record that contains an object, which is selected and a method is
called on it immediately.

source_file_program: IDENTIFIER DOT IDENTIFIER DOT IDENTIFIER LEFT_PARENTHESIS RIGHT_PARENTHESIS EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [method_call(top_level_block_expression):
              [selectable_inline_expression(top_level_block_expression):
                [selection(top_level_block_expression):
                  [selectable_inline_expression(top_level_block_expression):
                    [identifier: IDENTIFIER]
                  ]
                  DOT
                  [__: [option(NEWLINE):]]
                  [identifier: IDENTIFIER]
                ]
              ]
              DOT
              [__: [option(NEWLINE):]]
              [identifier: IDENTIFIER]
              LEFT_PARENTHESIS
              [__: [option(NEWLINE):]]
              [option(list_of(actual_parameter_or_index(top_level_block_expression),list_divider)):
              ]
              RIGHT_PARENTHESIS
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Record selection vs. method call.

source_file_program: IDENTIFIER DOT IDENTIFIER EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [selection(top_level_block_expression):
              [selectable_inline_expression(top_level_block_expression):
                [identifier: IDENTIFIER]
              ]
              DOT
              [__: [option(NEWLINE):]]
              [identifier: IDENTIFIER]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]

source_file_program: IDENTIFIER DOT IDENTIFIER LEFT_PARENTHESIS RIGHT_PARENTHESIS EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [method_call(top_level_block_expression):
              [selectable_inline_expression(top_level_block_expression):
                [identifier: IDENTIFIER]
              ]
              DOT
              [__: [option(NEWLINE):]]
              [identifier: IDENTIFIER]
              LEFT_PARENTHESIS
              [__: [option(NEWLINE):]]
              [option(list_of(actual_parameter_or_index(top_level_block_expression),list_divider)):
              ]
              RIGHT_PARENTHESIS
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Interaction between labeled data and function application/indexing/method call.

`foo not (bar)()

YES `foo not ((bar)())
NO  (`foo not (bar))()

source_file_program: LABEL NOT LEFT_PARENTHESIS IDENTIFIER RIGHT_PARENTHESIS LEFT_PARENTHESIS RIGHT_PARENTHESIS EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [literal(top_level_block_expression):
              LABEL
              [__: [option(NEWLINE):]]
              [labelable_inline_expression(top_level_block_expression):
                [unary_prefix_operation(top_level_block_expression):
                  NOT
                  [__: [option(NEWLINE):]]
                  [value_inline_expression(top_level_block_expression):
                    [application_or_indexing(top_level_block_expression):
                      [applicable_or_indexable_inline_expression(top_level_block_expression):
                        LEFT_PARENTHESIS
                        [__: [option(NEWLINE):]]
                        [block_of(top_level_block_expression):
                          [list_of(top_level_block_expression,block_divider):
                            [separated_nonempty_list(block_divider,top_level_block_expression):
                              [top_level_block_expression:
                                [identifier: IDENTIFIER]
                              ]
                            ]
                          ]
                        ]
                        RIGHT_PARENTHESIS
                      ]
                      LEFT_PARENTHESIS
                      [__: [option(NEWLINE):]]
                      [option(list_of(actual_parameter_or_index(top_level_block_expression),list_divider)):
                      ]
                      RIGHT_PARENTHESIS
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Interaction between labeled data and selection.

`foo not (bar).baz

YES `foo not ((bar).baz)
NO  (`foo not (bar)).baz

source_file_program: LABEL NOT LEFT_PARENTHESIS IDENTIFIER RIGHT_PARENTHESIS DOT IDENTIFIER EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [literal(top_level_block_expression):
              LABEL
              [__: [option(NEWLINE):]]
              [labelable_inline_expression(top_level_block_expression):
                [unary_prefix_operation(top_level_block_expression):
                  NOT
                  [__: [option(NEWLINE):]]
                  [value_inline_expression(top_level_block_expression):
                    [selection(top_level_block_expression):
                      [selectable_inline_expression(top_level_block_expression):
                        LEFT_PARENTHESIS
                        [__: [option(NEWLINE):]]
                        [block_of(top_level_block_expression):
                          [list_of(top_level_block_expression,block_divider):
                            [separated_nonempty_list(block_divider,top_level_block_expression):
                              [top_level_block_expression:
                                [identifier: IDENTIFIER]
                              ]
                            ]
                          ]
                        ]
                        RIGHT_PARENTHESIS
                      ]
                      DOT
                      [__: [option(NEWLINE):]]
                      [identifier: IDENTIFIER]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Labeling a labeled data.

`foo `bar baz

YES `foo (`bar baz)
NO  (`foo `bar) baz

source_file_program: LABEL LABEL IDENTIFIER EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [literal(top_level_block_expression):
              LABEL
              [__: [option(NEWLINE):]]
              [labelable_inline_expression(top_level_block_expression):
                [literal(top_level_block_expression):
                  LABEL
                  [__: [option(NEWLINE):]]
                  [labelable_inline_expression(top_level_block_expression):
                    [identifier: IDENTIFIER]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

(*
Interaction between labels and conjunction in patterns.

`foo bar and baz

YES (`foo bar) and baz
NO  `foo (bar and baz)

source_file_program: MATCH IDENTIFIER AS LABEL IDENTIFIER AND IDENTIFIER IN IDENTIFIER END EOF

ACCEPT
[source_file_program:
  [__: [option(NEWLINE):]]
  [option(block_of(top_level_block_expression)):
    [block_of(top_level_block_expression):
      [list_of(top_level_block_expression,block_divider):
        [separated_nonempty_list(block_divider,top_level_block_expression):
          [top_level_block_expression:
            [flow_control(top_level_block_expression):
              MATCH
              [__: [option(NEWLINE):]]
              [condition_inline_expression(top_level_block_expression):
                [identifier: IDENTIFIER]
              ]
              [__: [option(NEWLINE):]]
              [list(pattern_match_case(top_level_block_expression)):
                AS
                [__: [option(NEWLINE):]]
                [pattern:
                  [pattern:
                    LABEL
                    [__: [option(NEWLINE):]]
                    [labelable_pattern: [identifier: IDENTIFIER]]
                  ]
                  AND
                  [__: [option(NEWLINE):]]
                  [pattern: [identifier: IDENTIFIER]]
                ]
                [__: [option(NEWLINE):]]
                IN
                [__: [option(NEWLINE):]]
                [option(block_of(top_level_block_expression)):
                  [block_of(top_level_block_expression):
                    [list_of(top_level_block_expression,block_divider):
                      [separated_nonempty_list(block_divider,top_level_block_expression):
                        [top_level_block_expression: [identifier: IDENTIFIER]]
                      ]
                    ]
                  ]
                ]
                [list(pattern_match_case(top_level_block_expression)):]
              ]
              END
            ]
          ]
        ]
      ]
    ]
  ]
  EOF
]
*)

%%

(**************)
(* Public API *)
(**************)

(**
  A front-end for the BigBang parser library.
*)

(* open Batteries;; *)

(* open Big_bang_ast;; *)
(* open Big_bang_generated_lexer;; *)
(* open Big_bang_generated_parser;; *)
(* open Tiny_bang_parser_support;; *)

(* let parse_big_bang_expressions (input : IO.input) = *)
(*   let buf = Lexing.from_input input in *)
(*   let read_expr () = *)
(*     begin *)
(*       reset_ast_position_hash(); *)
(*       let result = *)
(*         Big_bang_generated_parser.delim_expr *)
(*           Big_bang_generated_lexer.token *)
(*           buf *)
(*       in *)
(*       let position_hash = Tiny_bang_parser_support.get_ast_position_hash () in *)
(*       match result with *)
(*         | Some(e) -> Some(e,position_hash) *)
(*         | None -> None *)
(*     end *)
(*   in *)
(*   LazyList.from_while read_expr;; *)

(* let parse_big_bang_program (input : IO.input) = *)
(*   let buf = Lexing.from_input input in *)
(*   reset_ast_position_hash(); *)
(*   let e = Big_bang_generated_parser.prog *)
(*             Big_bang_generated_lexer.token *)
(*             buf *)
(*   in *)
(*   let position_hash = Tiny_bang_parser_support.get_ast_position_hash() in *)
(*   (e,position_hash) *)
(* ;; *)

(*
let parse_big_bang_source_file_program (input : IO.input) : unit =
  input
  |> Lexing.from_input
  |> Big_bang_generated_parser.source_file_program Big_bang_generated_lexer.read
;;
*)
