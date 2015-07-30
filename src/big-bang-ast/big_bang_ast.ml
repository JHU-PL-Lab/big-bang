(**
   Contains data type definitions for the BigBang AST.
*)

open Batteries;;

open Tiny_bang_ast_uid;;

(******************************************************************************)

type identifier =
  | Identifier of string
;;

module Identifier_order =
struct
  type t = identifier
  let compare = compare
end;;

module Identifier_map = Map.Make(Identifier_order);;

(******************************************************************************)

type program =
  | Program of ast_uid * block

and block =
  | Block of ast_uid * expression list

(******************************************************************************)

and expression =
  | Expression_literal of ast_uid * literal
  | Expression_operation of ast_uid * operation
  | Expression_selection of ast_uid * expression * identifier
  | Expression_application_or_indexing of ast_uid * expression * expression list
  | Expression_method_call of ast_uid * expression * identifier * expression list
  | Expression_identifier of ast_uid * identifier
  | Expression_assignment of ast_uid * assignment
  | Expression_flow_control of ast_uid * flow_control
  | Expression_function_abstraction of ast_uid * identifier * identifier list * block
  | Expression_return of ast_uid * expression
  | Expression_block of ast_uid * block

(******************************************************************************)

and pattern =
  | Pattern_pattern_literal of ast_uid * pattern_literal
  | Pattern_pattern_operation of ast_uid * pattern_operation
  | Pattern_pattern_identifier of ast_uid * pattern_identifier
and pattern_literal =
  | Pattern_literal_primitive_literal of ast_uid * literal
  | Pattern_literal_pattern_record of ast_uid * pattern Identifier_map.t
  | Pattern_literal_pattern_list of ast_uid * pattern list * pattern_identifier option
  | Pattern_literal_pattern_labeled_data of ast_uid * identifier * pattern
and pattern_operation =
  | Pattern_operation_conjunction of ast_uid * pattern * pattern
and pattern_identifier =
  | Pattern_identifier of identifier
  | Pattern_identifier_catch_all

(******************************************************************************)

and literal =
  | Boolean of ast_uid * bool
  | Number of ast_uid * int
  | Character of ast_uid * char
  | Text of ast_uid * string
  | Empty_onion of ast_uid
  | Record of ast_uid * expression Identifier_map.t
  | List of ast_uid * expression list
  | Labeled_data of ast_uid * identifier * expression
  | Anonymous_function of ast_uid * identifier list * block
  | Object of ast_uid * object_section list

(******************************************************************************)

and object_section =
  | Object_include_section of ast_uid * block
  | Object_public_section of ast_uid * object_member list
  | Object_private_section of ast_uid * object_member list
and object_member =
  | Object_member_field_definition of ast_uid * assignment
  | Object_member_method_definition of ast_uid * identifier * identifier list * block

(******************************************************************************)

and operation =
  | Operation_and of ast_uid * expression * expression
  | Operation_or of ast_uid * expression * expression
  | Operation_xor of ast_uid * expression * expression
  | Operation_not of ast_uid * expression
  | Operation_plus of ast_uid * expression * expression
  | Operation_minus of ast_uid * expression * expression
  | Operation_multiplication of ast_uid * expression * expression
  | Operation_division of ast_uid * expression * expression
  | Operation_onioning of ast_uid * expression * expression
  | Operation_equality of ast_uid * expression * expression
  | Operation_inequality of ast_uid * expression * expression

(******************************************************************************)

and assignment =
  | Assignment_immutable of ast_uid * identifier * expression
  | Assignment_mutable of ast_uid * identifier * expression
  | Assignment_mutable_update of ast_uid * expression * expression

(******************************************************************************)

and flow_control =
  | Flow_control_if of ast_uid * expression * block * (expression * block) list * block
  | Flow_control_match of ast_uid * expression * (pattern * block) list
  | Flow_control_while of ast_uid * expression * block
;;
