let append_to_clause_list tiny_bang_ast_expr new_clauses =
  let Tiny_bang_ast.Expr (_, tiny_bang_ast_expr_clauses) = tiny_bang_ast_expr in
  Tiny_bang_ast.Expr (Tiny_bang_ast_uid.next_uid (), (BatList.append tiny_bang_ast_expr_clauses new_clauses))
;;

let merge left_pattern_filter_rules right_pattern_filter_rules =
  Tiny_bang_ast.Pvar_map.fold
    Tiny_bang_ast.Pvar_map.add
    left_pattern_filter_rules
    right_pattern_filter_rules
;;

let rec a_translate_expr little_bang_ast_expr binding_ident =
  match little_bang_ast_expr with
  | Little_bang_ast.Value_expr (_, little_bang_ast_value) ->
    Tiny_bang_ast.Expr (
      (Tiny_bang_ast_uid.next_uid ()), [
        Tiny_bang_ast.Clause (
          (Tiny_bang_ast_uid.next_uid ()),
          Tiny_bang_ast.Var (
            (Tiny_bang_ast_uid.next_uid ()),
            binding_ident,
            None
          ),
          Tiny_bang_ast.Value_redex (
            (Tiny_bang_ast_uid.next_uid ()), (a_translate_value little_bang_ast_value)
          )
        )
      ]
    )
  | Little_bang_ast.Label_expr (_, little_bang_ast_expr_label, little_bang_ast_expr_subexpression) ->
    let subexpression_binding_ident = Tiny_bang_ast.new_fresh_ident () in
    append_to_clause_list (
      a_translate_expr little_bang_ast_expr_subexpression subexpression_binding_ident
    ) (
      [
        Tiny_bang_ast.Clause (
          (Tiny_bang_ast_uid.next_uid ()),
          Tiny_bang_ast.Var (
            (Tiny_bang_ast_uid.next_uid ()),
            binding_ident,
            None
          ),
          Tiny_bang_ast.Value_redex (
            (Tiny_bang_ast_uid.next_uid ()),
            Tiny_bang_ast.Label_value (
              (Tiny_bang_ast_uid.next_uid ()),
              little_bang_ast_expr_label,
              Tiny_bang_ast.Var ((Tiny_bang_ast_uid.next_uid ()), subexpression_binding_ident, None)
            )
          )
        )
      ]
    )
  | Little_bang_ast.Onion_expr (_, little_bang_ast_expr_left_subexpression, little_bang_ast_expr_right_subexpression) ->
    let (little_bang_ast_expr_left_binding_ident, little_bang_ast_expr_right_binding_ident) =
      ((Tiny_bang_ast.new_fresh_ident ()), (Tiny_bang_ast.new_fresh_ident ()))
    in
    let (
      (Tiny_bang_ast.Expr (_, _)) as little_bang_ast_expr_left_subexpression_converted,
      (Tiny_bang_ast.Expr (_, little_bang_ast_expr_right_subexpression_clauses))
    )
      =
      (
        (a_translate_expr little_bang_ast_expr_left_subexpression little_bang_ast_expr_left_binding_ident),
        (a_translate_expr little_bang_ast_expr_right_subexpression little_bang_ast_expr_right_binding_ident)
      )
    in
    (append_to_clause_list
       (append_to_clause_list little_bang_ast_expr_left_subexpression_converted little_bang_ast_expr_right_subexpression_clauses)
       [
         Tiny_bang_ast.Clause (
           (Tiny_bang_ast_uid.next_uid ()),
           Tiny_bang_ast.Var (
             (Tiny_bang_ast_uid.next_uid ()),
             binding_ident,
             None
           ),
           Tiny_bang_ast.Value_redex (
             (Tiny_bang_ast_uid.next_uid ()),
             Tiny_bang_ast.Onion_value (
               (Tiny_bang_ast_uid.next_uid ()),
               Tiny_bang_ast.Var (
                 (Tiny_bang_ast_uid.next_uid ()),
                 little_bang_ast_expr_left_binding_ident,
                 None
               ),
               Tiny_bang_ast.Var (
                 (Tiny_bang_ast_uid.next_uid ()),
                 little_bang_ast_expr_right_binding_ident,
                 None
               )
             )
           )
         )
       ]
    )
  | Little_bang_ast.Appl_expr (_, little_bang_ast_expr_function_subexpression, little_bang_ast_expr_parameter_subexpression) ->
    let (little_bang_ast_expr_function_binding_ident, little_bang_ast_expr_parameter_binding_ident) =
      ((Tiny_bang_ast.new_fresh_ident ()), (Tiny_bang_ast.new_fresh_ident ()))
    in
    let (
      (Tiny_bang_ast.Expr (_, _)) as little_bang_ast_expr_function_subexpression_converted,
      (Tiny_bang_ast.Expr (_, little_bang_ast_expr_parameter_subexpression_clauses))
    )
      =
      (
        (a_translate_expr little_bang_ast_expr_function_subexpression little_bang_ast_expr_function_binding_ident),
        (a_translate_expr little_bang_ast_expr_parameter_subexpression little_bang_ast_expr_parameter_binding_ident)
      )
    in
    (append_to_clause_list
       (append_to_clause_list little_bang_ast_expr_function_subexpression_converted little_bang_ast_expr_parameter_subexpression_clauses)
       [
         Tiny_bang_ast.Clause (
           (Tiny_bang_ast_uid.next_uid ()),
           Tiny_bang_ast.Var (
             (Tiny_bang_ast_uid.next_uid ()),
             binding_ident,
             None
           ),
           Tiny_bang_ast.Appl_redex (
             (Tiny_bang_ast_uid.next_uid ()),
             Tiny_bang_ast.Var (
               (Tiny_bang_ast_uid.next_uid ()),
               little_bang_ast_expr_function_binding_ident,
               None
             ),
             Tiny_bang_ast.Var (
               (Tiny_bang_ast_uid.next_uid ()),
               little_bang_ast_expr_parameter_binding_ident,
               None
             )
           )
         )
       ]
    )
  | Little_bang_ast.Let_expr (
      _, Tiny_bang_ast.Var (_, little_bang_ast_expr_assigned_ident, _),
      little_bang_ast_expr_assignment,
      little_bang_ast_expr_body
    ) ->
    let (
      (Tiny_bang_ast.Expr (_, _)) as little_bang_ast_expr_assignment_converted,
      (Tiny_bang_ast.Expr (_, little_bang_ast_expr_body_clauses))
    )
      =
      (
        (a_translate_expr little_bang_ast_expr_assignment little_bang_ast_expr_assigned_ident),
        (a_translate_expr little_bang_ast_expr_body binding_ident)
      )
    in
    append_to_clause_list little_bang_ast_expr_assignment_converted little_bang_ast_expr_body_clauses
  | Little_bang_ast.Var_expr (
      _, Tiny_bang_ast.Var (_, little_bang_ast_expr_var_ident, _)
    ) ->
    Tiny_bang_ast.Expr (
      (Tiny_bang_ast_uid.next_uid ()), [
        Tiny_bang_ast.Clause (
          (Tiny_bang_ast_uid.next_uid ()),
          Tiny_bang_ast.Var (
            (Tiny_bang_ast_uid.next_uid ()),
            binding_ident,
            None
          ),
          Tiny_bang_ast.Var_redex (
            (Tiny_bang_ast_uid.next_uid ()),
            Tiny_bang_ast.Var (
              (Tiny_bang_ast_uid.next_uid ()),
              little_bang_ast_expr_var_ident,
              None
            )
          )
        )
      ]
    )

and a_translate_value little_bang_ast_value =
  match little_bang_ast_value with
  | Little_bang_ast.Empty_onion (_) ->
    Tiny_bang_ast.Empty_onion_value (Tiny_bang_ast_uid.next_uid ())
  | Little_bang_ast.Function (_, little_bang_ast_pattern, little_bang_ast_body) ->
    Tiny_bang_ast.Function_value (
      (Tiny_bang_ast_uid.next_uid ()),
      a_translate_pattern little_bang_ast_pattern (Tiny_bang_ast.new_fresh_ident ()),
      a_translate_expr little_bang_ast_body (Tiny_bang_ast.new_fresh_ident ())
    )

and a_translate_pattern little_bang_ast_pattern binding_ident =
  match little_bang_ast_pattern with
  | Little_bang_ast.Empty_pattern (_) ->
    Tiny_bang_ast.Pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      Tiny_bang_ast.Pvar (
        (Tiny_bang_ast_uid.next_uid ()),
        binding_ident
      ),
      (
        Tiny_bang_ast.Pvar_map.add
          (
            Tiny_bang_ast.Pvar (
              (Tiny_bang_ast_uid.next_uid ()),
              binding_ident
            )
          )
          (Tiny_bang_ast.Empty_filter (Tiny_bang_ast_uid.next_uid ()))
          Tiny_bang_ast.Pvar_map.empty
      )
    )
  | Little_bang_ast.Label_pattern (_, little_bang_ast_label, little_bang_ast_subpattern) ->
    let label_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let Tiny_bang_ast.Pattern (
        _,
        little_bang_ast_labeled_var,
        little_bang_ast_pattern_filter_rules
      )
      =
      a_translate_pattern little_bang_ast_subpattern label_ident
    in
    Tiny_bang_ast.Pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      Tiny_bang_ast.Pvar (
        (Tiny_bang_ast_uid.next_uid ()),
        binding_ident
      ),
      (
        Tiny_bang_ast.Pvar_map.add
          (
            Tiny_bang_ast.Pvar (
              (Tiny_bang_ast_uid.next_uid ()),
              binding_ident
            )
          )
          (Tiny_bang_ast.Label_filter (
              (Tiny_bang_ast_uid.next_uid ()),
              little_bang_ast_label,
              little_bang_ast_labeled_var
            )
          )
          little_bang_ast_pattern_filter_rules
      )
    )
  | Little_bang_ast.Conjunction_pattern (_, little_bang_ast_left_subpattern, little_bang_ast_right_subpattern) ->
    let left_subexpression_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let right_subexpression_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let (
      (
        Tiny_bang_ast.Pattern (
          _,
          little_bang_ast_left_labeled_var,
          little_bang_ast_left_pattern_filter_rules
        )
      ),
      (
        Tiny_bang_ast.Pattern (
          _,
          little_bang_ast_right_labeled_var,
          little_bang_ast_right_pattern_filter_rules
        )
      )
    )
      =
      (
        (a_translate_pattern little_bang_ast_left_subpattern left_subexpression_ident),
        (a_translate_pattern little_bang_ast_right_subpattern right_subexpression_ident)
      )
    in
    Tiny_bang_ast.Pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      Tiny_bang_ast.Pvar (
        (Tiny_bang_ast_uid.next_uid ()),
        binding_ident
      ),
      (
        Tiny_bang_ast.Pvar_map.add
          (
            Tiny_bang_ast.Pvar (
              (Tiny_bang_ast_uid.next_uid ()),
              binding_ident
            )
          )
          (Tiny_bang_ast.Conjunction_filter (
              (Tiny_bang_ast_uid.next_uid ()),
              little_bang_ast_left_labeled_var,
              little_bang_ast_right_labeled_var
            )
          )
          (merge little_bang_ast_left_pattern_filter_rules little_bang_ast_right_pattern_filter_rules)
      )
    )
  | Little_bang_ast.Var_pattern (_, Tiny_bang_ast.Var (_, little_bang_ast_var_ident, _)) ->
    let spurious_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    Tiny_bang_ast.Pattern (
      (Tiny_bang_ast_uid.next_uid ()),
      Tiny_bang_ast.Pvar (
        (Tiny_bang_ast_uid.next_uid ()),
        binding_ident
      ),
      (
        Tiny_bang_ast.Pvar_map.add
          (
            Tiny_bang_ast.Pvar (
              (Tiny_bang_ast_uid.next_uid ()),
              binding_ident
            )
          )
          (Tiny_bang_ast.Conjunction_filter (
              (Tiny_bang_ast_uid.next_uid ()),
              (
                Tiny_bang_ast.Pvar (
                  (Tiny_bang_ast_uid.next_uid ()),
                  spurious_ident
                )
              ),
              (
                Tiny_bang_ast.Pvar (
                  (Tiny_bang_ast_uid.next_uid ()),
                  little_bang_ast_var_ident
                )
              )
            )
          )
          (
            Tiny_bang_ast.Pvar_map.add
              (
                Tiny_bang_ast.Pvar (
                  (Tiny_bang_ast_uid.next_uid ()),
                  spurious_ident
                )
              )
              (Tiny_bang_ast.Empty_filter (Tiny_bang_ast_uid.next_uid ()))
              (
                Tiny_bang_ast.Pvar_map.add
                  (
                    Tiny_bang_ast.Pvar (
                      (Tiny_bang_ast_uid.next_uid ()),
                      little_bang_ast_var_ident
                    )
                  )
                  (Tiny_bang_ast.Empty_filter (Tiny_bang_ast_uid.next_uid ()))
                  Tiny_bang_ast.Pvar_map.empty
              )
          )
      )
    )
;;

let a_translate little_bang_ast_expr =
  a_translate_expr little_bang_ast_expr (Tiny_bang_ast.new_fresh_ident ())
;;
