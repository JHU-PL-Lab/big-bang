open Batteries;;

open Big_bang_translator_constants;;
open Big_bang_translator_helpers;;
open Tiny_bang_utils;;

(** Translate Big Bang AST into Little Bang AST.

    The structure of this file resembles the structure of Big Bang AST
    definition itself.

    Notes such as `Encoding <number>` are references to Zach's thesis, Building
    a Typed Scripting Language.

    Notes such as `Fig. 1' are references to Jed's paper, Encoding Classes and
    Modules in TinyBang. *)

let translate_identifier (identifier : Big_bang_ast.identifier) : Little_bang_ast.ident =
  match identifier with
  | Big_bang_ast.Identifier identifier_string ->
    Little_bang_ast.Ident identifier_string
  | Big_bang_ast.Fresh_identifier identifier_id ->
    Little_bang_ast.Fresh_ident identifier_id
;;

let translate_identifier_to_tiny_bang
    (identifier : Big_bang_ast.identifier)
  : Tiny_bang_ast.ident =
  match identifier with
  | Big_bang_ast.Identifier identifier_string ->
    Tiny_bang_ast.Ident identifier_string
  | Big_bang_ast.Fresh_identifier identifier_id ->
    Tiny_bang_ast.Fresh_ident identifier_id
;;

let rec translate_program (program : Big_bang_ast.program) : Little_bang_ast.expr =
  match program with
  | Big_bang_ast.Program (_, block) -> translate_block block

and translate_block (block : Big_bang_ast.block) : Little_bang_ast.expr =
  (* Instead of using Fig. 1, translate the sequencing that happens in blocks as:

     (|expression_1 ; expression_2|) =
     let _ = expression_1 in expression_2

     This makes for a more natural A-Translation from the generated Little Bang
     code to Tiny Bang.

     If the translation of `expression_1' is an assignment, we use that
     assignment, instead of creating another one. The body (the part after the
     `in') is going to be `expression_2'. *)
  match block with
  | Big_bang_ast.Block (_, expressions) ->
    begin
      match expressions with
      | [] ->
        void ()
      | [expression] ->
        translate_expression expression
      | expression :: expressions_rest ->
        begin
          let translated_expression = translate_expression expression in
          let translated_expressions_rest =
            translate_block @@
              Big_bang_ast.Block (
                Tiny_bang_ast_uid.next_uid (),
                expressions_rest
              )
          in
          match translated_expression with
          | Little_bang_ast.Let_expr (ast_uid, variable, value, _) ->
            Little_bang_ast.Let_expr (
              ast_uid,
              variable,
              value,
              translated_expressions_rest
            )
          | _ ->
            Little_bang_ast.Let_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.new_fresh_ident ()
              ),
              translated_expression,
              translated_expressions_rest
            )
        end
    end

and translate_expression (expression : Big_bang_ast.expression) : Little_bang_ast.expr =
  match expression with
  | Big_bang_ast.Expression_literal (_, literal) ->
    translate_literal literal
  | Big_bang_ast.Expression_operation (_, operation) ->
    translate_operation operation

  (* Encoding 8.5 *)
  | Big_bang_ast.Expression_selection (_, subject, Big_bang_ast.Identifier selection_identifier_string) ->
    let fresh_identifier = Little_bang_ast.new_fresh_ident () in
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Function (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident selection_identifier_string
            ),
            Little_bang_ast.Var_pattern (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var (
                Tiny_bang_ast_uid.next_uid (),
                fresh_identifier
              )
            )
          ),
          Little_bang_ast.Var_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Var (
              Tiny_bang_ast_uid.next_uid (),
              fresh_identifier
            )
          )
        )
      ),
      translate_expression subject
    )

  (* Encoding 8.7 *)
  | Big_bang_ast.Expression_application_or_indexing (_, subject, actual_parameters) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      translate_expression subject,
      translate_actual_parameters actual_parameters
    )

  (* Encoding 8.9 *)
  | Big_bang_ast.Expression_method_call (
      _,
      receiver,
      Big_bang_ast.Identifier message,
      actual_parameters
    ) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      translate_expression receiver,
      Little_bang_ast.Onion_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Label_expr (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            Tiny_bang_ast.Ident "message"
          ),
          Little_bang_ast.Label_expr (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident message
            ),
            Little_bang_ast.Value_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Empty_onion (
                Tiny_bang_ast_uid.next_uid ()
              )
            )
          )
        ),
        translate_actual_parameters actual_parameters
      )
    )

  | Big_bang_ast.Expression_identifier (_, identifier) ->
    Little_bang_ast.Var_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Var (
        Tiny_bang_ast_uid.next_uid (),
        translate_identifier identifier
      )
    )

  | Big_bang_ast.Expression_flow_control (_, flow_control) ->
    translate_flow_control flow_control

  (*
  (|
  fun foo(bar) =
    bar + 1
  end
  |) =
  (|
  let foo = Y(
      fun (foo) =
        fun (bar) =
          bar + 1
        end
      end
    )
  |)
  *)
  | Big_bang_ast.Expression_function_abstraction (_, name, formal_parameters, body) ->
    translate_expression @@
    Big_bang_ast.Expression_assignment (
      Tiny_bang_ast_uid.next_uid (),
      Big_bang_ast.Assignment_immutable (
        Tiny_bang_ast_uid.next_uid (),
        name,
        y_combinator @@
        Big_bang_ast.Expression_literal (
          Tiny_bang_ast_uid.next_uid (),
          Big_bang_ast.Anonymous_function (
            Tiny_bang_ast_uid.next_uid (),
            [name],
            Big_bang_ast.Block (
              Tiny_bang_ast_uid.next_uid (),
              [
                Big_bang_ast.Expression_literal (
                  Tiny_bang_ast_uid.next_uid (),
                  Big_bang_ast.Anonymous_function (
                    Tiny_bang_ast_uid.next_uid (),
                    formal_parameters,
                    body
                  )
                )
              ]
            )
          )
        )
      )
    )

  | Big_bang_ast.Expression_block (_, block) ->
    translate_block block

  | _ -> raise @@ Not_yet_implemented "translate_expression"

and translate_actual_parameters (actual_parameters : Big_bang_ast.expression list) : Little_bang_ast.expr =
  actual_parameters
  |> List.mapi
    (
      fun parameter_index actual_parameter ->
        Little_bang_ast.Label_expr (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            Tiny_bang_ast.Ident (string_of_int parameter_index)
          ),
          translate_expression actual_parameter
        )
    )
  |> fold_onion

and translate_pattern (pattern : Big_bang_ast.pattern) : Little_bang_ast.pattern =
  match pattern with
  | Big_bang_ast.Pattern_pattern_literal (_, pattern_literal) ->
    translate_pattern_literal pattern_literal
  | Big_bang_ast.Pattern_pattern_operation (_, pattern_operation) ->
    translate_pattern_operation pattern_operation
  | Big_bang_ast.Pattern_pattern_identifier (_, pattern_identifier) ->
    translate_pattern_identifier pattern_identifier

and translate_pattern_literal (pattern_literal : Big_bang_ast.pattern_literal) : Little_bang_ast.pattern =
  match pattern_literal with

  (* Encoding 8.4 *)
  | Big_bang_ast.Pattern_literal_pattern_record (_, fields) ->
    fields
    |> Big_bang_ast.Identifier_map.enum
    |> Enum.map (
      fun (identifier, pattern) ->
        Little_bang_ast.Label_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            translate_identifier_to_tiny_bang identifier
          ),
          translate_pattern pattern
        )
    )
    |> List.of_enum
    |> fold_conjunction

  (* Encoding 8.6

     FIXME: This implementation of the list pattern rest can be misleading:

     (|[*rest]|) = rest

     In this case `rest' can match anything, no only lists.

     The reason for this is that we don't have in Little Bang pattern grammar a
     way to express lists, `forall type. fixpoint x. `Head type & `Tail x | `Nil
     () '. We lack fixpoints and disjunction patterns. *)
  | Big_bang_ast.Pattern_literal_pattern_list (_, patterns, pattern_rest_option) ->
    let rec translate_list (patterns : Big_bang_ast.pattern list) : Little_bang_ast.pattern =
      match patterns with
      | [] ->
        begin
          match pattern_rest_option with
          | Some pattern_rest ->
            translate_pattern_identifier pattern_rest
          | None ->
            Little_bang_ast.Label_pattern (
              Tiny_bang_ast_uid.next_uid (),
              Tiny_bang_ast.Label (
                Tiny_bang_ast.Ident "Nil"
              ),
              Little_bang_ast.Empty_pattern (
                Tiny_bang_ast_uid.next_uid ()
              )
            )
        end
      | pattern :: patterns_rest ->
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "Hd"
            ),
            translate_pattern pattern
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "Tl"
            ),
            translate_list patterns_rest
          )
        )
    in
    translate_list patterns

  | Big_bang_ast.Pattern_literal_pattern_labeled_data (
      _,
      Big_bang_ast.Identifier identifier_string,
      pattern
    ) ->
    Little_bang_ast.Label_pattern (
      Tiny_bang_ast_uid.next_uid (),
      Tiny_bang_ast.Label (
        Tiny_bang_ast.Ident identifier_string
      ),
      translate_pattern pattern
    )

  | _ -> raise @@ Not_yet_implemented "translate_pattern_literal"

and translate_pattern_operation (pattern_operation : Big_bang_ast.pattern_operation) : Little_bang_ast.pattern =
  match pattern_operation with
  | Big_bang_ast.Pattern_operation_conjunction (_, left_pattern, right_pattern) ->
    Little_bang_ast.Conjunction_pattern (
      Tiny_bang_ast_uid.next_uid (),
      translate_pattern left_pattern,
      translate_pattern right_pattern
    )

and translate_pattern_identifier (pattern_identifier : Big_bang_ast.pattern_identifier) : Little_bang_ast.pattern =
  match pattern_identifier with
  | Big_bang_ast.Pattern_identifier (identifier) ->
    Little_bang_ast.Var_pattern (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Var (
        Tiny_bang_ast_uid.next_uid (),
        translate_identifier identifier
      )
    )

  | Big_bang_ast.Pattern_identifier_catch_all ->
    Little_bang_ast.Var_pattern (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Var (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.new_fresh_ident ()
      )
    )


and translate_literal (literal : Big_bang_ast.literal) : Little_bang_ast.expr =
  match literal with

  (* Encoding 8.1 *)
  | Big_bang_ast.Boolean (_, boolean) ->
    begin
      match boolean with
      | true -> little_bang_true ()
      | false -> little_bang_false ()
    end

  | Big_bang_ast.Empty_onion (_) ->
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )

  (* Encoding 8.3 *)
  | Big_bang_ast.Record (_, fields) ->
    fields
    |> Big_bang_ast.Identifier_map.enum
    |> Enum.map (
      fun (identifier, expression) ->
        Little_bang_ast.Label_expr (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            translate_identifier_to_tiny_bang identifier
          ),
          translate_expression expression
        )
    )
    |> List.of_enum
    |> fold_onion

  (* Based on Encoding 8.6. In addition to the encoding presented there, this
     implementation has to deal with indexing. The solution consists in onioning
     a function the right of the structure so that it can be "applied":

     (|[]|) =
     (|
     `Nil () &
     fun (index) =
       runtime_error
     end
     |)

     (|[1, 2]|) =
     (|
     let head = 1
     let tail = [2]
     `Hd head &
     `Tl tail &
     fun (index) =
       if index =? 0
         head
       else
         tail(index - 1)
       end
     end
     |) *)
  | Big_bang_ast.List (_, expressions) ->
    let index_identifier = Big_bang_ast_utils.new_fresh_identifier () in
    begin
      match expressions with
      | [] ->
        translate_operation @@
        Big_bang_ast.Operation_onioning (
          Tiny_bang_ast_uid.next_uid (),
          Big_bang_ast.Expression_literal (
            Tiny_bang_ast_uid.next_uid (),
            Big_bang_ast.Labeled_data (
              Tiny_bang_ast_uid.next_uid (),
              Big_bang_ast.Identifier "Nil",
              Big_bang_ast.Expression_literal (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Empty_onion (
                  Tiny_bang_ast_uid.next_uid ()
                )
              )
            )
          ),
          Big_bang_ast.Expression_literal (
            Tiny_bang_ast_uid.next_uid (),
            Big_bang_ast.Anonymous_function (
              Tiny_bang_ast_uid.next_uid (),
              [index_identifier],
              Big_bang_ast.Block (
                Tiny_bang_ast_uid.next_uid (),
                [
                  runtime_error ()
                ]
              )
            )
          )
        )
      | expression :: expressions_rest ->
        let head_identifier = Big_bang_ast_utils.new_fresh_identifier () in
        let tail_identifier = Big_bang_ast_utils.new_fresh_identifier () in
        translate_block @@
        Big_bang_ast.Block (
          Tiny_bang_ast_uid.next_uid (),
          [
            Big_bang_ast.Expression_assignment (
              Tiny_bang_ast_uid.next_uid (),
              Big_bang_ast.Assignment_immutable (
                Tiny_bang_ast_uid.next_uid (),
                head_identifier,
                expression
              )
            );
            Big_bang_ast.Expression_assignment (
              Tiny_bang_ast_uid.next_uid (),
              Big_bang_ast.Assignment_immutable (
                Tiny_bang_ast_uid.next_uid (),
                tail_identifier,
                Big_bang_ast.Expression_literal (
                  Tiny_bang_ast_uid.next_uid (),
                  Big_bang_ast.List (
                    Tiny_bang_ast_uid.next_uid (),
                    expressions_rest
                  )
                )
              )
            );
            fold_big_bang_onion [
              Big_bang_ast.Expression_literal (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Labeled_data (
                  Tiny_bang_ast_uid.next_uid (),
                  Big_bang_ast.Identifier "Hd",
                  Big_bang_ast.Expression_identifier (
                    Tiny_bang_ast_uid.next_uid (),
                    head_identifier
                  )
                )
              );
              Big_bang_ast.Expression_literal (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Labeled_data (
                  Tiny_bang_ast_uid.next_uid (),
                  Big_bang_ast.Identifier "Tl",
                  Big_bang_ast.Expression_identifier (
                    Tiny_bang_ast_uid.next_uid (),
                    tail_identifier
                  )
                )
              );
              Big_bang_ast.Expression_literal (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Anonymous_function (
                  Tiny_bang_ast_uid.next_uid (),
                  [index_identifier],
                  Big_bang_ast.Block (
                    Tiny_bang_ast_uid.next_uid (),
                    [
                      Big_bang_ast.Expression_flow_control (
                        Tiny_bang_ast_uid.next_uid (),
                        Big_bang_ast.Flow_control_if (
                          Tiny_bang_ast_uid.next_uid (),
                          Big_bang_ast.Expression_operation (
                            Tiny_bang_ast_uid.next_uid (),
                            Big_bang_ast.Operation_equality (
                              Tiny_bang_ast_uid.next_uid (),
                              Big_bang_ast.Expression_identifier (
                                Tiny_bang_ast_uid.next_uid (),
                                index_identifier
                              ),
                              Big_bang_ast.Expression_literal (
                                Tiny_bang_ast_uid.next_uid (),
                                Big_bang_ast.Number (
                                  Tiny_bang_ast_uid.next_uid (),
                                  0
                                )
                              )
                            )
                          ),
                          Big_bang_ast.Block (
                            Tiny_bang_ast_uid.next_uid (),
                            [
                              Big_bang_ast.Expression_identifier (
                                Tiny_bang_ast_uid.next_uid (),
                                head_identifier
                              )
                            ]
                          ),
                          [],
                          Big_bang_ast.Block (
                            Tiny_bang_ast_uid.next_uid (),
                            [
                              Big_bang_ast.Expression_application_or_indexing (
                                Tiny_bang_ast_uid.next_uid (),
                                Big_bang_ast.Expression_identifier (
                                  Tiny_bang_ast_uid.next_uid (),
                                  tail_identifier
                                ),
                                [
                                  Big_bang_ast.Expression_operation (
                                    Tiny_bang_ast_uid.next_uid (),
                                    Big_bang_ast.Operation_minus (
                                      Tiny_bang_ast_uid.next_uid (),
                                      Big_bang_ast.Expression_identifier (
                                        Tiny_bang_ast_uid.next_uid (),
                                        index_identifier
                                      ),
                                      Big_bang_ast.Expression_literal (
                                        Tiny_bang_ast_uid.next_uid (),
                                        Big_bang_ast.Number (
                                          Tiny_bang_ast_uid.next_uid (),
                                          1
                                        )
                                      )
                                    )
                                  )
                                ]
                              )
                            ]
                          )
                        )
                      )
                    ]
                  )
                )
              )
            ]
          ]
        )
    end

  | Big_bang_ast.Labeled_data (
      _,
      Big_bang_ast.Identifier identifier_string,
      expression
    ) ->
    Little_bang_ast.Label_expr (
      Tiny_bang_ast_uid.next_uid (),
      Tiny_bang_ast.Label (
        Tiny_bang_ast.Ident identifier_string
      ),
      translate_expression expression
    )

  (* Encoding 8.8 with Section 8.5.3 Extra Arguments handling *)
  | Big_bang_ast.Anonymous_function (_, formal_parameters, body) ->
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        formal_parameters
        |> List.mapi
          (
            fun parameter_index formal_parameter ->
              Little_bang_ast.Label_pattern (
                Tiny_bang_ast_uid.next_uid (),
                Tiny_bang_ast.Label (
                  Tiny_bang_ast.Ident (string_of_int parameter_index)
                ),
                Little_bang_ast.Var_pattern (
                  Tiny_bang_ast_uid.next_uid (),
                  Little_bang_ast.Var (
                    Tiny_bang_ast_uid.next_uid (),
                    translate_identifier formal_parameter
                  )
                )
              )
          )
        |> fold_conjunction,
        translate_block body
      )
    )
    |> prevent_extra_parameters @@ List.length formal_parameters

  (* Encoding 8.9

     It's necessary to reverse the list of translated expressions from object
     sections before onioning the results because later declarations should
     overwrite previous declarations and, therefore, should be on the left of
     the onion. *)
  | Big_bang_ast.Object (_, object_sections) ->
    object_sections
    |> List.map translate_object_section
    |> List.concat
    |> List.rev
    |> fold_onion
    |> seal

  | _ -> raise @@ Not_yet_implemented "translate_literal"

and translate_object_section (object_section : Big_bang_ast.object_section) : Little_bang_ast.expr list =
  match object_section with
  | Big_bang_ast.Object_public_section (_, object_members) ->
    object_members
    |> List.map translate_object_member

  | Big_bang_ast.Object_include_section (_, Big_bang_ast.Block (_, expressions)) ->
    expressions
    |> List.map translate_expression

  | _ -> raise @@ Not_yet_implemented "translate_object_section"

and translate_object_member (object_member : Big_bang_ast.object_member) : Little_bang_ast.expr =
  match object_member with

  (* Encoding 8.9 *)
  | Big_bang_ast.Object_member_field_definition (_, raw_assignment) ->
    begin
      let translated_assignment = translate_assignment raw_assignment in
      match translated_assignment with
      | Little_bang_ast.Let_expr (
          _,
          Little_bang_ast.Var (
            _,
            Little_bang_ast.Ident identifier_string
          ),
          value,
          _
        ) ->
        Little_bang_ast.Label_expr (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            Tiny_bang_ast.Ident identifier_string
          ),
          value
        )
      | _ -> raise @@ Invariant_failure "`translate_assignment' didn't return an appropriate `Tiny_bang_ast.expr'."
    end

  (* Encoding 8.9 with Section 8.5.3 Extra Arguments handling *)
  | Big_bang_ast.Object_member_method_definition (
      _,
      message,
      formal_parameters,
      body
    ) ->
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        [
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "message"
            ),
            Little_bang_ast.Label_pattern (
              Tiny_bang_ast_uid.next_uid (),
              Tiny_bang_ast.Label (
                translate_identifier_to_tiny_bang message
              ),
              Little_bang_ast.Empty_pattern (
                Tiny_bang_ast_uid.next_uid ()
              )
            )
          );
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "self"
            ),
            Little_bang_ast.Var_pattern (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Ident "self"
              )
            )
          )
        ]
        @
        (
          formal_parameters
          |> List.mapi
            (
              fun parameter_index formal_parameter ->
                Little_bang_ast.Label_pattern (
                  Tiny_bang_ast_uid.next_uid (),
                  Tiny_bang_ast.Label (
                    Tiny_bang_ast.Ident (string_of_int parameter_index)
                  ),
                  Little_bang_ast.Var_pattern (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Var (
                      Tiny_bang_ast_uid.next_uid (),
                      translate_identifier formal_parameter
                    )
                  )
                )
            )
        )
        |> fold_conjunction,
        translate_block body
      )
    )
    |> prevent_extra_parameters @@ List.length formal_parameters

and translate_operation (operation : Big_bang_ast.operation) : Little_bang_ast.expr =
  match operation with

  (* Encoding 8.2 *)
  | Big_bang_ast.Operation_and (_, left_operand, right_operand) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      and_truth_table (),
      translate_actual_parameters [left_operand; right_operand]
    )

  (* Encoding 8.2 *)
  | Big_bang_ast.Operation_or (_, left_operand, right_operand) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      or_truth_table (),
      translate_actual_parameters [left_operand; right_operand]
    )

  (* Based on Encoding 8.2 *)
  | Big_bang_ast.Operation_xor (_, left_operand, right_operand) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      xor_truth_table (),
      translate_actual_parameters [left_operand; right_operand]
    )

  (* Encoding 8.2 *)
  | Big_bang_ast.Operation_not (_, operand) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      not_truth_table (),
      translate_expression operand
    )

  | Big_bang_ast.Operation_onioning (_, left_expression, right_expression) ->
    Little_bang_ast.Onion_expr (
      Tiny_bang_ast_uid.next_uid (),
      translate_expression left_expression,
      translate_expression right_expression
    )

  | _ -> raise @@ Not_yet_implemented "translate_operation"

(* The body (the part after the `in') is always `Void (). `translate_block` is
   responsible for populating it, if necessary. *)
and translate_assignment (assignment : Big_bang_ast.assignment) : Little_bang_ast.expr =
  match assignment with
  | Big_bang_ast.Assignment_immutable (_, variable, value) ->
    Little_bang_ast.Let_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Var (
        Tiny_bang_ast_uid.next_uid (),
        translate_identifier variable
      ),
      translate_expression value,
      void ()
    )
  | _ -> raise @@ Not_yet_implemented "translate_assignment"

and translate_flow_control (flow_control : Big_bang_ast.flow_control) : Little_bang_ast.expr =
  match flow_control with

(*
Adapted from Fig. 1

(|
if condition_1
  block_1
else if condition_2
  block_2
else
  block_3
end
|) =
(|
if condition_1
  block_1
else
  if condition_2
    block_2
  else
    block_3
  end
end
|)
*)
  | Big_bang_ast.Flow_control_if (_, condition, then_block, else_ifs, else_block) ->
    begin
      match else_ifs with
      | [] ->
        Little_bang_ast.Appl_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Onion_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Value_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Function (
                Tiny_bang_ast_uid.next_uid (),
                little_bang_true_pattern (),
                translate_block then_block
              )
            ),
            Little_bang_ast.Value_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Function (
                Tiny_bang_ast_uid.next_uid (),
                little_bang_false_pattern (),
                translate_block else_block
              )
            )
          ),
          translate_expression condition
        )

      | (else_if_condition, else_if_block) :: else_ifs_rest ->
        translate_flow_control @@
        Big_bang_ast.Flow_control_if (
          Tiny_bang_ast_uid.next_uid (),
          condition,
          then_block,
          [],
          Big_bang_ast.Block (
            Tiny_bang_ast_uid.next_uid (),
            [
              Big_bang_ast.Expression_flow_control (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Flow_control_if (
                  Tiny_bang_ast_uid.next_uid (),
                  else_if_condition,
                  else_if_block,
                  else_ifs_rest,
                  else_block
                )
              )
            ]
          )
        )
    end

(*
(|
match subject
as pattern_1 in block_1
as pattern_2 in block_2
end
|) =
(
  (fun (|pattern_1|) -> (|block_1|)) &
  (fun (|pattern_2|) -> (|block_2|))
)(subject)

If there are no `as...in...' branches, the translation tries to apply the empty
onion, which is going to typefail whenever this code is touched.
*)
  | Big_bang_ast.Flow_control_match (_, subject, clauses) ->
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      clauses
      |> List.map
        (
          fun (pattern, block) ->
            Little_bang_ast.Value_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Function (
                Tiny_bang_ast_uid.next_uid (),
                translate_pattern pattern,
                translate_block block
              )
            )
        )
      |> fold_onion,
      translate_expression subject
    )

(*
(|
repeat while condition
  block
end
|) =
(|
fun repeat () =
  if condition
    block
    repeat ()
  end
end
repeat ()
|)
*)
  | Big_bang_ast.Flow_control_while (_, condition, block) ->
    let repeat_identifier = Big_bang_ast_utils.new_fresh_identifier () in
    translate_block @@
    Big_bang_ast.Block (
      Tiny_bang_ast_uid.next_uid (),
      [
        Big_bang_ast.Expression_function_abstraction (
          Tiny_bang_ast_uid.next_uid (),
          repeat_identifier,
          [],
          Big_bang_ast.Block (
            Tiny_bang_ast_uid.next_uid (),
            [
              Big_bang_ast.Expression_flow_control (
                Tiny_bang_ast_uid.next_uid (),
                Big_bang_ast.Flow_control_if (
                  Tiny_bang_ast_uid.next_uid (),
                  condition,
                  Big_bang_ast.Block (
                    Tiny_bang_ast_uid.next_uid (),
                    [
                      Big_bang_ast.Expression_block (
                        Tiny_bang_ast_uid.next_uid (),
                        block
                      );
                      Big_bang_ast.Expression_application_or_indexing (
                        Tiny_bang_ast_uid.next_uid (),
                        Big_bang_ast.Expression_identifier (
                          Tiny_bang_ast_uid.next_uid (),
                          repeat_identifier
                        ),
                        []
                      )
                    ]
                  ),
                  [],
                  Big_bang_ast.Block (
                    Tiny_bang_ast_uid.next_uid (),
                    []
                  )
                )
              )
            ]
          )
        );
        Big_bang_ast.Expression_application_or_indexing (
          Tiny_bang_ast_uid.next_uid (),
          Big_bang_ast.Expression_identifier (
            Tiny_bang_ast_uid.next_uid (),
            repeat_identifier
          ),
          []
        )
      ]
    )
;;
