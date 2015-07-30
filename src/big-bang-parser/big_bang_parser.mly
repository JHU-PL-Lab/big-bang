%{
open Big_bang_ast;;
open Tiny_bang_ast_uid;;
open Tiny_bang_parser_support;;
open Tiny_bang_source_origin;;
open Lexing;;


(* FIXME: Refactor the repetition between this, Little_bang_generated_parser and
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
;;
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

%start <Big_bang_ast.program>        source_file_program
%start <Big_bang_ast.program option> toploop_program

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
  | __;
    expressions_option = option(block_of(top_level_block_expression));
    EOF
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      Program (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

%public
toploop_program:
  | __;
    expressions_option = option(block_of(top_level_block_expression));
    toploop_program_terminator
    {
      match expressions_option with
      | Some expressions ->
        Some (
          Program (
            next_uid $startpos $endpos,
            Block (next_uid $startpos $endpos, expressions)
          )
        )
      | None -> None
    }
  ;

%inline
toploop_program_terminator:
  | DOUBLE_SEMICOLON
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
  | expression = literal(top_level_block_expression)
  | expression = operation(top_level_block_expression)
  | expression = selection(top_level_block_expression)
  | expression = application_or_indexing(top_level_block_expression)
  | expression = method_call(top_level_block_expression)
  | expression = identifier_expression
  | expression = assignment_expression(top_level_block_expression)
  | expression = flow_control(top_level_block_expression)
  | expression = function_abstraction
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(top_level_block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

%public
function_or_method_body_block_expression:
  | expression = literal(function_or_method_body_block_expression)
  | expression = operation(function_or_method_body_block_expression)
  | expression = selection(function_or_method_body_block_expression)
  | expression = application_or_indexing(function_or_method_body_block_expression)
  | expression = method_call(function_or_method_body_block_expression)
  | expression = identifier_expression
  | expression = assignment_expression(function_or_method_body_block_expression)
  | expression = flow_control(function_or_method_body_block_expression)
  | expression = function_abstraction
  | expression = return(function_or_method_body_block_expression)
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(function_or_method_body_block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* `value_block_expression' was designed for the `include' section of objects
   where a list (block) of values is required.

   IMPORTANT: It should be kept in sync with `value_inline_expression'! *)
%public
value_block_expression:
  | expression = literal(value_block_expression)
  | expression = operation(value_block_expression)
  | expression = selection(value_block_expression)
  | expression = application_or_indexing(value_block_expression)
  | expression = method_call(value_block_expression)
  | expression = identifier_expression
  | expression = flow_control(value_block_expression)
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(value_block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* `value_inline_expression' can be used where a value is expected. For example, on
   label values, record elements and as actual parameters.

   IMPORTANT: It should be kept in sync with `value_block_expression'! *)
%public
value_inline_expression(block_expression):
  | expression = literal(block_expression)
  | expression = operation(block_expression)
  | expression = selection(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = method_call(block_expression)
  | expression = identifier_expression
  | expression = flow_control(block_expression)
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* Conditional expressions are those which may appear in the condition part of
   e.g. an if or while expression. *)
%public
condition_inline_expression(block_expression):
  | expression = literal(block_expression)
  | expression = operation(block_expression)
  | expression = selection(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = method_call(block_expression)
  | expression = identifier_expression
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* Selectable expressions are those that can show up in selections (to the left
   of the `.'). *)
%public
selectable_inline_expression(block_expression):
  | expression = delimited_literal(block_expression)
  | expression = selection(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = method_call(block_expression)
  | expression = identifier_expression
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* Assignable expressions are those that can show up in mutable update
   assignments (to the left of the `='). *)
%public
assignable_inline_expression(block_expression):
  | expression = selection(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = identifier_expression
    { expression }
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
  | expression = delimited_literal(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = method_call(block_expression)
  | expression = identifier_expression
  | expression = flow_control(block_expression)
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(* Labelable expressions are those that can be labeled (after the label itself). *)
%public
labelable_inline_expression(block_expression):
  | expression = literal(block_expression)
  | expression = unary_prefix_operation_expression(block_expression)
  | expression = selection(block_expression)
  | expression = application_or_indexing(block_expression)
  | expression = method_call(block_expression)
  | expression = identifier_expression
  | expression = flow_control(block_expression)
    { expression }
  | LEFT_PARENTHESIS;
    __;
    expressions = block_of(block_expression);
    RIGHT_PARENTHESIS
    {
      Expression_block (
        next_uid $startpos $endpos,
        Block (next_uid $startpos $endpos, expressions)
      )
    }
  ;

(******************************************************************************)

(* Pattern rules are named after their expression correlates with the `pattern_'
  prefix. *)

%public
pattern:
  | pattern = pattern_literal
  | pattern = pattern_operation
  | pattern = pattern_pattern_identifier
  | LEFT_PARENTHESIS;
    __;
    pattern = pattern;
    __;
    RIGHT_PARENTHESIS
    { pattern }
  ;

(* Labelable patterns are those that can be labeled (after the label itself). *)
%public
pattern_labelable:
  | pattern = pattern_literal
  | pattern = pattern_pattern_identifier
  | LEFT_PARENTHESIS;
    __;
    pattern = pattern;
    __;
    RIGHT_PARENTHESIS
    { pattern }
  ;

%public
pattern_literal:
  | pattern = pattern_primitive_literal
  | pattern = pattern_structured_literal
    {
      Pattern_pattern_literal (
        next_uid $startpos $endpos,
        pattern
      )
    }
  ;

%inline
pattern_primitive_literal:
  | literal = primitive_literal
    {
      Pattern_literal_primitive_literal (
        next_uid $startpos $endpos,
        literal
      )
    }
  ;

%inline
pattern_structured_literal:
  | pattern = pattern_delimited_structured_literal
  | pattern = pattern_open_ended_structured_literal
    { pattern }
  ;

%inline
pattern_delimited_structured_literal:
  | pattern = pattern_record
  | pattern = pattern_list
    { pattern }
  ;

%inline
pattern_open_ended_structured_literal:
  | pattern = pattern_prefix_structured_literal
    { pattern }
  ;

%inline
pattern_prefix_structured_literal:
  | pattern = pattern_labeled_data
    { pattern }
  ;

%inline
pattern_record:
  | LEFT_CURLY_BRACKET;
    __;
    pattern_record_fields_option = option(list_of(pattern_record_field, list_divider));
    RIGHT_CURLY_BRACKET
    {
      let pattern_record_fields =
        match pattern_record_fields_option with
        | Some pattern_record_fields ->
          Identifier_map.of_enum @@ Batteries.List.enum pattern_record_fields
        | None -> Identifier_map.empty
      in
      Pattern_literal_pattern_record (
        next_uid $startpos $endpos,
        pattern_record_fields
      )
    }
  ;

%inline
pattern_record_field:
  | key = identifier;
    __;
    EQUALS;
    __;
    value = pattern
    { (key, value) }
  ;

%inline
pattern_list:
  | LEFT_SQUARE_BRACKET;
    __;
    patterns_option = option(list_of(pattern_list_field, list_divider));
    rest = option(terminated(pattern_list_field_remainder, __));
    RIGHT_SQUARE_BRACKET
    {
      let patterns =
        match patterns_option with
        | Some patterns -> patterns
        | None -> []
      in
      Pattern_literal_pattern_list (
        next_uid $startpos $endpos,
        patterns,
        rest
      )
    }
  ;

%inline
pattern_list_field:
  | pattern = pattern
    { pattern }
  ;

%inline
pattern_list_field_remainder:
  | ASTERISK;
    (* NO NEWLINE *);
    pattern_identifier = pattern_identifier
    { pattern_identifier }
  ;

%inline
pattern_labeled_data:
  | label = label;
    __;
    pattern = pattern_labelable
    {
      Pattern_literal_pattern_labeled_data (
        next_uid $startpos $endpos,
        label,
        pattern
      )
    }
  ;

%public
pattern_operation:
  | pattern_operation = pattern_binary_operation
    {
      Pattern_pattern_operation (
        next_uid $startpos $endpos,
        pattern_operation
      )
    }
  ;

%inline
pattern_binary_operation:
  | left_operand = pattern;
    (* NO NEWLINE *);
    pattern_operation_constructor = pattern_binary_operator;
    __;
    right_operand = pattern
    {
      pattern_operation_constructor left_operand right_operand
    }
  ;

%inline
pattern_binary_operator:
  | pattern_operator = pattern_conjunction_operator
    { pattern_operator }
  ;

%inline
pattern_conjunction_operator:
  | AND
    {
      fun left_operand right_operand ->
        Pattern_operation_conjunction (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  ;

%public
pattern_pattern_identifier:
  | pattern_identifier = pattern_identifier
    {
      Pattern_pattern_identifier (
        next_uid $startpos $endpos,
        pattern_identifier
      )
    }
  ;

%inline
pattern_identifier:
  | identifier = identifier
    { Pattern_identifier identifier }
  | pattern_catch_all_identifier
    { Pattern_identifier_catch_all }
  ;

%inline
pattern_catch_all_identifier:
  | UNDERSCORE
    { () }
  ;

(******************************************************************************)

%public
literal(block_expression):
  | literal = primitive_literal
  | literal = structured_literal(block_expression)
    {
      Expression_literal (
        next_uid $startpos $endpos,
        literal
      )
    }
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
  | delimited_literal = primitive_literal
  | delimited_literal = delimited_structured_literal(block_expression)
    {
      Expression_literal (
        next_uid $startpos $endpos,
        delimited_literal
      )
    }
  ;

%inline
primitive_literal:
  | primitive_literal = boolean
  | primitive_literal = number
  | primitive_literal = character
  | primitive_literal = text
  | primitive_literal = empty_onion
    { primitive_literal }
  ;

%inline
structured_literal(block_expression):
  | structured_literal = delimited_structured_literal(block_expression)
  | structured_literal = open_ended_structured_literal(block_expression)
    { structured_literal }
  ;

%inline
delimited_structured_literal(block_expression):
  | delimited_structured_literal = record(block_expression)
  | delimited_structured_literal = list_(block_expression)
  | delimited_structured_literal = anonymous_function
  | delimited_structured_literal = object_
    { delimited_structured_literal }
  ;

%inline
open_ended_structured_literal(block_expression):
  | open_ended_structured_literal = prefix_structured_literal(block_expression)
    { open_ended_structured_literal }
  ;

%inline
prefix_structured_literal(block_expression):
  | prefix_structured_literal = labeled_data(block_expression)
    { prefix_structured_literal }
  ;

%inline
boolean:
  | TRUE
    { Boolean (next_uid $startpos $endpos, true) }
  | FALSE
    { Boolean (next_uid $startpos $endpos, false) }
 ;

%inline
number:
  | number = integer
    { number }
  ;

%inline
integer:
  | integer = INTEGER
    { Number (next_uid $startpos $endpos, integer) }
  ;

%inline
character:
  | character = CHARACTER
    { Character (next_uid $startpos $endpos, character) }
  ;

%inline
text:
  | text = TEXT
    { Text (next_uid $startpos $endpos, text) }
  ;

%inline
empty_onion:
  | EMPTY_ONION
    { Empty_onion (next_uid $startpos $endpos) }
  ;

%inline
record(block_expression):
  | LEFT_CURLY_BRACKET;
    __;
    record_fields_option = option(list_of(record_field(block_expression), list_divider));
    RIGHT_CURLY_BRACKET
    {
      let record_fields =
        match record_fields_option with
        | Some record_fields ->
            Identifier_map.of_enum @@ Batteries.List.enum record_fields
        | None -> Identifier_map.empty
      in
      Record (
        next_uid $startpos $endpos,
        record_fields
      )
    }
  ;

%inline
record_field(block_expression):
  | key = identifier;
    __;
    EQUALS;
    __;
    value = value_inline_expression(block_expression)
    { (key, value) }
  ;

(* It's very important that this rule is called `list_' instead of just
`list', as Menhir comes with a helper function called `list' and we don't want
to shadow it. *)
%inline
list_(block_expression):
  | LEFT_SQUARE_BRACKET;
    __;
    expressions_option = option(list_of(list_field(block_expression), list_divider));
    RIGHT_SQUARE_BRACKET
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      List (
        next_uid $startpos $endpos,
        expressions
      )
    }
  ;

%inline
list_field(block_expression):
  | expression = value_inline_expression(block_expression)
    { expression }
  ;

%inline
labeled_data(block_expression):
  | label = label;
    __;
    expression = labelable_inline_expression(block_expression)
    {
      Labeled_data (
        next_uid $startpos $endpos,
        label,
        expression
      )
    }
  ;

%inline
label:
  | label = LABEL
    { Identifier label }
  ;

(******************************************************************************)

%inline
anonymous_function:
  | FUN;
    __;
    formal_parameters = formal_parameters;
    __;
    EQUALS;
    __;
    expressions_option = option(block_of(function_or_method_body_block_expression));
    END
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      Anonymous_function (
        next_uid $startpos $endpos,
        formal_parameters,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

%inline
formal_parameters:
  | LEFT_PARENTHESIS;
    __;
    formal_parameters_option = option(list_of(formal_parameter, list_divider));
    RIGHT_PARENTHESIS
    {
      match formal_parameters_option with
      | Some formal_parameters -> formal_parameters
      | None -> []
    }
  ;

%inline
formal_parameter:
  | identifier = identifier
    { identifier }
  ;

(******************************************************************************)

%inline
object_:
  | OBJECT;
    __;
    object_sections = list(object_section);
    END
    {
      Object (
        next_uid $startpos $endpos,
        object_sections
      )
    }
  ;

%inline
object_section:
  | object_section = object_include_section
  | object_section = object_member_section
    { object_section }
  ;

%inline
object_include_section:
  | INCLUDE;
    __;
    object_include_section_body = object_include_section_body
    {
      Object_include_section (
        next_uid $startpos $endpos,
        Block (
          next_uid $startpos $endpos,
          object_include_section_body
        )
      )
    }
  ;

%inline
object_include_section_body:
  | expressions_option = option(block_of(value_block_expression))
    {
      match expressions_option with
      | Some expressions -> expressions
      | None -> []
    }
  ;

%inline
object_member_section:
  | object_member_section_constructor = object_member_section_header;
    __;
    object_member_section_body = object_member_section_body
    { object_member_section_constructor object_member_section_body }
  ;

%inline
object_member_section_header:
  | PUBLIC
    {
      fun object_member_section_body ->
        Object_public_section (
          next_uid $startpos $endpos,
          object_member_section_body
        )
    }
  | PRIVATE
    {
      fun object_member_section_body ->
        Object_private_section (
          next_uid $startpos $endpos,
          object_member_section_body
        )
    }
  ;

%inline
object_member_section_body:
  | object_members_option = option(block_of(object_member))
    {
      match object_members_option with
      | Some object_members -> object_members
      | None -> []
    }
  ;

%inline
object_member:
  | object_member = object_field_definition
  | object_member = method_definition
    { object_member }
  ;

%inline
object_field_definition:
  | assignment = immutable_assignment(value_block_expression)
  | assignment = mutable_assignment(value_block_expression)
    {
      Object_member_field_definition (
        next_uid $startpos $endpos,
        assignment
      )
    }
  ;

%inline
method_definition:
  | DEF;
    __;
    name = identifier;
    (* NO NEWLINE *);
    formal_parameters = formal_parameters;
    __;
    EQUALS;
    __;
    expressions_option = option(block_of(function_or_method_body_block_expression));
    END
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      Object_member_method_definition (
        next_uid $startpos $endpos,
        name,
        formal_parameters,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

(******************************************************************************)

%public
operation(block_expression):
  | operation = binary_operation(block_expression)
  | operation = unary_prefix_operation(block_expression)
    {
      Expression_operation (
        next_uid $startpos $endpos,
        operation
      )
    }
  ;

%public
unary_prefix_operation_expression(block_expression):
  | operation = unary_prefix_operation(block_expression)
    {
      Expression_operation (
        next_uid $startpos $endpos,
        operation
      )
    }
  ;

%inline
binary_operation(block_expression):
  | left_operand = value_inline_expression(block_expression);
    (* NO NEWLINE *);
    operation_constructor = binary_operator;
    __;
    right_operand = value_inline_expression(block_expression)
    { operation_constructor left_operand right_operand }
  ;

%inline
binary_operator:
  | binary_operator = primitive_binary_operator
  | binary_operator = structured_binary_operator
  | binary_operator = general_binary_operator
    { binary_operator }
  ;

%inline
primitive_binary_operator:
  | primitive_binary_operator = boolean_binary_operator
  | primitive_binary_operator = number_binary_operator
    { primitive_binary_operator }
  ;

%inline
structured_binary_operator:
  | structured_binary_operator = onioning_operator
    { structured_binary_operator }
  ;

%inline
general_binary_operator:
  | general_binary_operator = comparison_operator
    { general_binary_operator }
  ;

%inline
boolean_binary_operator:
  | AND
    {
      fun left_operand right_operand ->
        Operation_and (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | OR
    {
      fun left_operand right_operand ->
        Operation_or (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | XOR
    {
      fun left_operand right_operand ->
        Operation_xor (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  ;

%inline
number_binary_operator:
  | PLUS
    {
      fun left_operand right_operand ->
        Operation_plus (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | MINUS
    {
      fun left_operand right_operand ->
        Operation_minus (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | ASTERISK
    {
      fun left_operand right_operand ->
        Operation_multiplication (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | SLASH
    {
      fun left_operand right_operand ->
        Operation_division (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  ;

%inline
onioning_operator:
  | AMPERSAND
    {
      fun left_operand right_operand ->
        Operation_onioning (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  ;

%inline
comparison_operator:
  | OPERATOR_EQUALITY
    {
      fun left_operand right_operand ->
        Operation_equality (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  | OPERATOR_INEQUALITY
    {
      fun left_operand right_operand ->
        Operation_inequality (
          next_uid $startpos $endpos,
          left_operand,
          right_operand
        )
    }
  ;

(* %inline *)
unary_prefix_operation(block_expression):
  | operation_constructor = unary_prefix_operator;
    __;
    operand = value_inline_expression(block_expression)
    { operation_constructor operand }
    %prec UNARY_PREFIX_OPERATION_PRECEDENCE
  ;

%inline
unary_prefix_operator:
  | unary_prefix_operator = primitive_unary_prefix_operator
    { unary_prefix_operator }
  ;

%inline
primitive_unary_prefix_operator:
  | primitive_unary_prefix_operator = boolean_unary_prefix_operator
    { primitive_unary_prefix_operator }
  ;

%inline
boolean_unary_prefix_operator:
  | NOT
    {
      fun operand ->
        Operation_not (
          next_uid $startpos $endpos,
          operand
        )
    }
  ;

(******************************************************************************)

(* Selection works on records and objects (for fields, not methods). *)
%public
selection(block_expression):
  | expression = selectable_inline_expression(block_expression);
    DOT;
    __;
    identifier = identifier
    {
      Expression_selection (
        next_uid $startpos $endpos,
        expression,
        identifier
      )
    }
  ;

(******************************************************************************)

(* This rule conflates the syntax for function application and indexing
   (an operation) in texts and lists. They are spelled the same. *)
%public
application_or_indexing(block_expression):
  | expression = applicable_or_indexable_inline_expression(block_expression);
    actual_parameters_or_indexes = actual_parameters_or_indexes(block_expression)
    {
      Expression_application_or_indexing (
        next_uid $startpos $endpos,
        expression,
        actual_parameters_or_indexes
      )
    }
  ;

%public
method_call(block_expression):
  | expression = selectable_inline_expression(block_expression);
    DOT;
    __;
    identifier = identifier;
    (* NO NEWLINE *);
    actual_parameters_or_indexes = actual_parameters_or_indexes(block_expression)
    {
      Expression_method_call (
        next_uid $startpos $endpos,
        expression,
        identifier,
        actual_parameters_or_indexes
      )
    }
  ;

%inline
actual_parameters_or_indexes(block_expression):
  | LEFT_PARENTHESIS;
    __;
    actual_parameters_or_indexes_option = option(list_of(actual_parameter_or_index(block_expression), list_divider));
    RIGHT_PARENTHESIS
    {
      match actual_parameters_or_indexes_option with
      | Some actual_parameters_or_indexes -> actual_parameters_or_indexes
      | None -> []
    }
  ;

%inline
actual_parameter_or_index(block_expression):
  | expression = value_inline_expression(block_expression)
    { expression }
  ;

(******************************************************************************)

%public
identifier_expression:
  | identifier = identifier
    {
      Expression_identifier (
        next_uid $startpos $endpos,
        identifier
      )
    }
  ;

%inline
identifier:
  | identifier = IDENTIFIER
    { Identifier identifier }
  ;

(******************************************************************************)

%public
assignment_expression(block_expression):
  | assignment = assignment(block_expression)
    {
      Expression_assignment (
        next_uid $startpos $endpos,
        assignment
      )
    }

%inline
assignment(block_expression):
  | assignment = immutable_assignment(block_expression)
  | assignment = mutable_assignment(block_expression)
  | assignment = mutable_update_assignment(block_expression)
    { assignment }
  ;

%inline
immutable_assignment(block_expression):
  | LET;
    __;
    unqualified_assignment = unqualified_assignment(block_expression)
    {
      let (identifier, expression) = unqualified_assignment in
      Assignment_immutable (
        next_uid $startpos $endpos,
        identifier,
        expression
      )
    }
  ;

%inline
mutable_assignment(block_expression):
  | REF;
    __;
    unqualified_assignment = unqualified_assignment(block_expression)
    {
      let (identifier, expression) = unqualified_assignment in
      Assignment_mutable (
        next_uid $startpos $endpos,
        identifier,
        expression
      )
    }
  ;

%inline
unqualified_assignment(block_expression):
  | identifier = identifier;
    (* NO NEWLINE *);
    EQUALS;
    __;
    expression = value_inline_expression(block_expression)
    { (identifier, expression) }
  ;

%inline
mutable_update_assignment(block_expression):
  | left_expression = assignable_inline_expression(block_expression);
    (* NO NEWLINE *);
    EQUALS;
    __;
    right_expression = value_inline_expression(block_expression)
    {
      Assignment_mutable_update (
        next_uid $startpos $endpos,
        left_expression,
        right_expression
      )
    }
  ;

(******************************************************************************)

%public
flow_control(block_expression):
  | flow_control = conditional(block_expression)
  | flow_control = repetition(block_expression)
    {
      Expression_flow_control (
        next_uid $startpos $endpos,
        flow_control
      )
    }
  ;

%inline
conditional(block_expression):
  | conditional = if_(block_expression)
  | conditional = pattern_match(block_expression)
    { conditional }
  ;

%inline
if_(block_expression):
  | IF;
    __;
    condition = condition_inline_expression(block_expression);
    then_;
    then_expressions_option = option(block_of(block_expression));
    else_ifs = list(else_if(block_expression));
    else_expressions_option = option(else_(block_expression));
    END
    {
      let then_expressions =
        match then_expressions_option with
        | Some then_expressions -> then_expressions
        | None -> []
      in
      let else_expressions =
        match else_expressions_option with
        | Some else_expressions -> else_expressions
        | None ->
          Block (
            next_uid $startpos $endpos,
            []
          )
      in
      Flow_control_if (
        next_uid $startpos $endpos,
        condition,
        Block (
          next_uid $startpos $endpos,
          then_expressions
        ),
        else_ifs,
        else_expressions
      )
    }
  ;

%inline
else_if(block_expression):
  | ELSE_IF;
    __;
    condition = condition_inline_expression(block_expression);
    then_;
    expressions_option = option(block_of(block_expression))
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      (
        condition,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

%inline
else_(block_expression):
  | ELSE;
    __;
    expressions_option = option(block_of(block_expression))
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
        Block (
          next_uid $startpos $endpos,
          expressions
        )
    }
  ;

%inline
pattern_match(block_expression):
  | MATCH;
    __;
    subject = condition_inline_expression(block_expression);
    __;
    branches = list(pattern_match_case(block_expression))
    END
    {
      Flow_control_match (
        next_uid $startpos $endpos,
        subject,
        branches
      )
    }
  ;

%inline
pattern_match_case(block_expression):
  | AS;
    __;
    pattern = pattern;
    __;
    IN;
    __;
    expressions_option = option(block_of(block_expression))
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      (
        pattern,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

%inline
repetition(block_expression):
  | REPEAT;
    __;
    repetition = while_(block_expression)
    { repetition }
  ;

%inline
while_(block_expression):
  | WHILE;
    __;
    condition = condition_inline_expression(block_expression);
    do_;
    expressions_option = option(block_of(block_expression));
    END
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      Flow_control_while (
        next_uid $startpos $endpos,
        condition,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

(******************************************************************************)

%public
function_abstraction:
  | FUN;
    __;
    identifier = identifier;
    (* NO NEWLINE *);
    formal_parameters = formal_parameters;
    __;
    EQUALS;
    __;
    expressions_option = option(block_of(function_or_method_body_block_expression));
    END
    {
      let expressions =
        match expressions_option with
        | Some expressions -> expressions
        | None -> []
      in
      Expression_function_abstraction (
        next_uid $startpos $endpos,
        identifier,
        formal_parameters,
        Block (
          next_uid $startpos $endpos,
          expressions
        )
      )
    }
  ;

(******************************************************************************)

%public
return(block_expression):
  | RETURN;
    __;
    expression = value_inline_expression(block_expression)
    {
      Expression_return (
        next_uid $startpos $endpos,
        expression
      )
    }
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
  | THEN __
    { () }
  ;

(* `do_' is used to separate the condition from the consequence
   on the `repeat' expressions. *)
%public
do_:
  | NEWLINE
  | DO __
    { () }
  ;

(******************************************************************************)

%public
block_of(expression):
  | expressions = list_of(expression, block_divider)
    { expressions }
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
  | elements = nonempty_list(terminated(element, separator))
  | elements = separated_nonempty_list(separator, element)
      { elements }
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
