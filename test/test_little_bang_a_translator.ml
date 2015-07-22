open OUnit2;;

let test_expression_empty_onion _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          )
        ]
      ) ->
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the empty onion" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_label _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Label_expr (
      Tiny_bang_ast_uid.next_uid (),
      Tiny_bang_ast.Label (
        Tiny_bang_ast.Ident "foo"
      ),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              tiny_bang_subexpression_ident_1,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Label_value (_,
                                            Tiny_bang_ast.Label (Tiny_bang_ast.Ident "foo"),
                                            Tiny_bang_ast.Var (_, tiny_bang_subexpression_ident_2, None)
                                           )
            )
          )
        ]
      ) ->
      assert_equal tiny_bang_subexpression_ident_1 tiny_bang_subexpression_ident_2;
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the label expression" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_onion _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Onion_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      ),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              left_binding_ident_1,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              right_binding_ident_1,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Onion_value (
                _,
                Tiny_bang_ast.Var (_, left_binding_ident_2, None),
                Tiny_bang_ast.Var (_, right_binding_ident_2, None)
              )
            )
          )
        ]
      ) ->
      assert_equal left_binding_ident_1 left_binding_ident_2;
      assert_equal right_binding_ident_1 right_binding_ident_2;
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the onion" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_function _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ()),
        Little_bang_ast.Value_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
        )
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Function_value (
                _,
                Tiny_bang_ast.Pattern (_, _ , _),
                Tiny_bang_ast.Expr (
                  _,
                  [
                    Tiny_bang_ast.Clause (
                      _,
                      _,
                      Tiny_bang_ast.Value_redex (
                        _, Tiny_bang_ast.Empty_onion_value (_)
                      )
                    )
                  ]
                )
              )
            )
          )
        ]
      ) ->
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the function" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_appl _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Appl_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Function (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ()),
          Little_bang_ast.Value_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
          )
        )
      ),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              function_binding_ident_1,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Function_value (
                _,
                Tiny_bang_ast.Pattern (_, _ , _),
                Tiny_bang_ast.Expr (
                  _,
                  [
                    Tiny_bang_ast.Clause (
                      _,
                      _,
                      Tiny_bang_ast.Value_redex (
                        _, Tiny_bang_ast.Empty_onion_value (_)
                      )
                    )
                  ]
                )
              )
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              parameter_binding_ident_1,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Appl_redex (
              _,
              Tiny_bang_ast.Var (_, function_binding_ident_2, None),
              Tiny_bang_ast.Var (_, parameter_binding_ident_2, None)
            )
          )
        ]
      ) ->
      assert_equal function_binding_ident_1 function_binding_ident_2;
      assert_equal parameter_binding_ident_1 parameter_binding_ident_2;
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the appl" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_let _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let assigned_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Let_expr (
      Tiny_bang_ast_uid.next_uid (),
      Tiny_bang_ast.Var (
        (Tiny_bang_ast_uid.next_uid ()),
        assigned_ident,
        None
      ),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      ),
      Little_bang_ast.Value_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Empty_onion (Tiny_bang_ast_uid.next_uid ())
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              assigned_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          );
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Value_redex (
              _, Tiny_bang_ast.Empty_onion_value (_)
            )
          )
        ]
      ) ->
      assert_equal assigned_ident assigned_ident_output;
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the let" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_expression_var _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let var_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_expression =
    Little_bang_ast.Var_expr (
      Tiny_bang_ast_uid.next_uid (),
      Tiny_bang_ast.Var (
        (Tiny_bang_ast_uid.next_uid ()),
        var_ident,
        None
      )
    )
  in
  let is_expected_tiny_bang_expression tiny_bang_expression =
    match tiny_bang_expression with
    | Tiny_bang_ast.Expr (
        _, [
          Tiny_bang_ast.Clause (
            _, Tiny_bang_ast.Var (
              _,
              binding_ident_output,
              None
            ),
            Tiny_bang_ast.Var_redex (
              _, Tiny_bang_ast.Var (
                _,
                var_ident_output,
                None
              )
            )
          )
        ]
      ) ->
      assert_equal var_ident var_ident_output;
      assert_equal binding_ident binding_ident_output;
      true
    | _ -> false
  in
  assert_bool "expected A-Translation to return the var" (
    is_expected_tiny_bang_expression (
      Little_bang_a_translator.a_translate_expr little_bang_expression binding_ident
    )
  )
;;

let test_pattern_empty_onion _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_pattern =
    Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ())
  in
  match Little_bang_a_translator.a_translate_pattern little_bang_pattern binding_ident with
  | Tiny_bang_ast.Pattern (
      _, (Tiny_bang_ast.Var (_, binding_ident_output, None)), tiny_bang_pattern_filter_rules
    ) ->
    assert_equal binding_ident binding_ident_output;
    (
      match Tiny_bang_ast.Var_map.bindings tiny_bang_pattern_filter_rules with
      | [(Tiny_bang_ast.Var (_, binding_ident_output, None) as binding_var, _)] ->
        assert_equal binding_ident binding_ident_output;
        (
          match Tiny_bang_ast.Var_map.find binding_var tiny_bang_pattern_filter_rules with
          | Tiny_bang_ast.Empty_filter _ -> ()
          | _ -> assert_failure "expected A-translation of pattern to have the right structure"
        )
      | _ -> assert_failure "expected A-translation of pattern to have the right structure"
    )
  | _ -> assert_failure "expected A-translation of pattern to have the right structure"
;;

let test_pattern_label _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let label_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_pattern =
    Little_bang_ast.Label_pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      (Tiny_bang_ast.Label label_ident),
      (Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ()))
    )
  in
  match Little_bang_a_translator.a_translate_pattern little_bang_pattern binding_ident with
  | Tiny_bang_ast.Pattern (
      _, (Tiny_bang_ast.Var (_, binding_ident_output, None)), tiny_bang_pattern_filter_rules
    ) ->
    assert_equal binding_ident binding_ident_output;
    (
      match Tiny_bang_ast.Var_map.bindings tiny_bang_pattern_filter_rules with
      | [
        (Tiny_bang_ast.Var (_, binding_ident_output, None) as binding_var, _);
        (Tiny_bang_ast.Var (_, _, None) as label_var, _)
      ] ->
        assert_equal binding_ident binding_ident_output;
        (
          match (
            (Tiny_bang_ast.Var_map.find binding_var tiny_bang_pattern_filter_rules),
            (Tiny_bang_ast.Var_map.find label_var tiny_bang_pattern_filter_rules)
          ) with
          | (
            (Tiny_bang_ast.Label_filter (_, (Tiny_bang_ast.Label label_ident_output), _)),
            (Tiny_bang_ast.Empty_filter _)
          ) -> assert_equal label_ident label_ident_output
          | _ -> assert_failure "expected A-translation of pattern to have the right structure"
        )
      | _ -> assert_failure "expected A-translation of pattern to have the right structure"
    )
  | _ -> assert_failure "expected A-translation of pattern to have the right structure"
;;

let test_pattern_conjunction _ =
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_pattern =
    Little_bang_ast.Conjunction_pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      (Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ())),
      (Little_bang_ast.Empty_pattern (Tiny_bang_ast_uid.next_uid ()))
    )
  in
  match Little_bang_a_translator.a_translate_pattern little_bang_pattern binding_ident with
  | Tiny_bang_ast.Pattern (
      _, (Tiny_bang_ast.Var (_, binding_ident_output, None)), tiny_bang_pattern_filter_rules
    ) ->
    assert_equal binding_ident binding_ident_output;
    (
      match Tiny_bang_ast.Var_map.bindings tiny_bang_pattern_filter_rules with
      | [
        (Tiny_bang_ast.Var (_, binding_ident_output, None) as binding_var, _);
        (Tiny_bang_ast.Var (_, _, None) as conjunction_left_var, _);
        (Tiny_bang_ast.Var (_, _, None) as conjunction_right_var, _)
      ] ->
        assert_equal binding_ident binding_ident_output;
        (
          match (
            (Tiny_bang_ast.Var_map.find binding_var tiny_bang_pattern_filter_rules),
            (Tiny_bang_ast.Var_map.find conjunction_left_var tiny_bang_pattern_filter_rules),
            (Tiny_bang_ast.Var_map.find conjunction_right_var tiny_bang_pattern_filter_rules)
          ) with
          | (
            (Tiny_bang_ast.Conjunction_filter (_, _, _)),
            (Tiny_bang_ast.Empty_filter _),
            (Tiny_bang_ast.Empty_filter _)
          ) -> ()
          | _ -> assert_failure "expected A-translation of pattern to have the right structure"
        )
      | _ -> assert_failure "expected A-translation of pattern to have the right structure"
    )
  | _ -> assert_failure "expected A-translation of pattern to have the right structure"
;;

let test_pattern_var _ =
  let var_ident = Tiny_bang_ast.new_fresh_ident () in
  let binding_ident = Tiny_bang_ast.new_fresh_ident () in
  let little_bang_pattern =
    Little_bang_ast.Var_pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      (
        Tiny_bang_ast.Var (
          (Tiny_bang_ast_uid.next_uid ()),
          var_ident,
          None
        )
      )
    )
  in
  match Little_bang_a_translator.a_translate_pattern little_bang_pattern binding_ident with
  | Tiny_bang_ast.Pattern (
      _, (Tiny_bang_ast.Var (_, binding_ident_output, None)), tiny_bang_pattern_filter_rules
    ) ->
    assert_equal binding_ident binding_ident_output;
    (
      match Tiny_bang_ast.Var_map.bindings tiny_bang_pattern_filter_rules with
      | [
        (Tiny_bang_ast.Var (_, var_right_ident, None) as var_right_var, _);
        (Tiny_bang_ast.Var (_, binding_ident_output, None) as binding_var, _);
        (Tiny_bang_ast.Var (_, var_left_ident, None) as var_left_var, _)
      ] ->
        assert_equal binding_ident binding_ident_output;
        (
          match (
            (Tiny_bang_ast.Var_map.find binding_var tiny_bang_pattern_filter_rules),
            (Tiny_bang_ast.Var_map.find var_left_var tiny_bang_pattern_filter_rules),
            (Tiny_bang_ast.Var_map.find var_right_var tiny_bang_pattern_filter_rules)
          ) with
          | (
            (Tiny_bang_ast.Conjunction_filter (
                _,
                Tiny_bang_ast.Var (_, var_left_ident_output, None),
                Tiny_bang_ast.Var (_, var_right_ident_output, None)
              )
            ),
            (Tiny_bang_ast.Empty_filter _),
            (Tiny_bang_ast.Empty_filter _)
          ) ->
            assert_equal var_left_ident var_left_ident_output;
            assert_equal var_right_ident var_right_ident_output
          | _ -> assert_failure "expected A-translation of pattern to have the right structure"
        )
      | _ -> assert_failure "expected A-translation of pattern to have the right structure"
    )
  | _ -> assert_failure "expected A-translation of pattern to have the right structure"
;;

let tests = "Test_little_bang_a_translator" >:::
            [
              "test_expression_empty_onion" >:: test_expression_empty_onion;
              "test_expression_label" >:: test_expression_label;
              "test_expression_onion" >:: test_expression_onion;
              "test_expression_function" >:: test_expression_function;
              "test_expression_appl" >:: test_expression_appl;
              "test_expression_let" >:: test_expression_let;
              "test_expression_var" >:: test_expression_var;
              "test_pattern_empty_onion" >:: test_pattern_empty_onion;
              "test_pattern_label" >:: test_pattern_label;
              "test_pattern_conjunction" >:: test_pattern_conjunction;
              "test_pattern_var" >:: test_pattern_var
            ]
;;
