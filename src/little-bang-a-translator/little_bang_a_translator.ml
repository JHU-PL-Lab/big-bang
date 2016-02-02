open Batteries;;
open List;;


let ident_compare a b = match (a, b) with
    | (Little_bang_ast.Ident x, Little_bang_ast.Ident y) -> String.compare x y 
    | (Little_bang_ast.Fresh_ident x, Little_bang_ast.Fresh_ident y) -> Int.compare x y
    | (Little_bang_ast.Ident _, Little_bang_ast.Fresh_ident _) -> -1
    | (Little_bang_ast.Fresh_ident _, Little_bang_ast.Ident _) -> 1
;;


module Ident_order =
struct
  type t = Little_bang_ast.ident
  let compare = ident_compare
end;;

module Ident_map = Map.Make(Ident_order);;

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

let naive_a_translate_ident i =
  match i with
  | Little_bang_ast.Ident s -> Tiny_bang_ast.Ident s
  | Little_bang_ast.Fresh_ident n ->
    (* This step makes sense because the fresh idents from the LittleBang AST
       module should be in sync with the ident counter from the TinyBang AST
       module. *)
    Tiny_bang_ast.Fresh_ident (n, None)
;;

let a_translate_ident bindings i = if Ident_map.mem i bindings then
    Ident_map.find i bindings else naive_a_translate_ident i

let derive_fresh_ident ident = 
    match ident with
        | Little_bang_ast.Ident label -> Tiny_bang_ast.new_labeled_fresh_ident label
        | Little_bang_ast.Fresh_ident idx ->
                Tiny_bang_ast.new_labeled_fresh_ident @@ "originally_" ^
                (string_of_int idx)
;;

let rec a_translate_expr bindings little_bang_ast_expr binding_ident =
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
            (Tiny_bang_ast_uid.next_uid ()), (a_translate_value bindings little_bang_ast_value)
          )
        )
      ]
    )
  | Little_bang_ast.Label_expr (_, little_bang_ast_expr_label, little_bang_ast_expr_subexpression) ->
    let subexpression_binding_ident = Tiny_bang_ast.new_fresh_ident () in
    append_to_clause_list (
      a_translate_expr bindings little_bang_ast_expr_subexpression subexpression_binding_ident
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
        (a_translate_expr bindings little_bang_ast_expr_left_subexpression little_bang_ast_expr_left_binding_ident),
        (a_translate_expr bindings little_bang_ast_expr_right_subexpression little_bang_ast_expr_right_binding_ident)
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
        (a_translate_expr bindings little_bang_ast_expr_function_subexpression little_bang_ast_expr_function_binding_ident),
        (a_translate_expr bindings little_bang_ast_expr_parameter_subexpression little_bang_ast_expr_parameter_binding_ident)
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
  | Little_bang_ast.Ref_expr (_, value) ->
    let ident = Tiny_bang_ast.new_fresh_ident ()
    in
    append_to_clause_list (a_translate_expr bindings value ident) [
      Tiny_bang_ast.Clause (
        Tiny_bang_ast_uid.next_uid (),
        Tiny_bang_ast.Var (Tiny_bang_ast_uid.next_uid (), binding_ident, None),
        Tiny_bang_ast.Value_redex (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Ref_value (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Var (Tiny_bang_ast_uid.next_uid (), ident, None)
          )
        )
      );
    ]
  | Little_bang_ast.Builtin_expr (_, op, args) ->
      (* This function composition is used to provoke the side effect of next_uid
         for each argument*)
      let arg_idents = map (Tiny_bang_ast.new_fresh_ident % const ()) args
      in
      let arg_exprs = map (uncurry @@ a_translate_expr bindings) (map2 (fun x y -> (x, y)) args arg_idents)
      in
      append_to_clause_list
        (fold_left
          (fun x y ->
            match y with
            | Tiny_bang_ast.Expr (_, clauses) -> append_to_clause_list x clauses
          )
          (hd arg_exprs) (tl arg_exprs))
        [Tiny_bang_ast.Clause (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Var ((Tiny_bang_ast_uid.next_uid ()), binding_ident, None),
          Tiny_bang_ast.Builtin_redex (
            (Tiny_bang_ast_uid.next_uid ()), op,
              (map (fun x -> Tiny_bang_ast.Var (Tiny_bang_ast_uid.next_uid (), x, None)) arg_idents)
          )
        )]
  | Little_bang_ast.Let_expr (
      _, Little_bang_ast.Var (_, little_bang_ast_expr_assigned_ident),
      little_bang_ast_expr_assignment,
      little_bang_ast_expr_body
    ) ->
    let destination = derive_fresh_ident little_bang_ast_expr_assigned_ident in
    let bindings' =
        Ident_map.add little_bang_ast_expr_assigned_ident destination bindings
    in
    (*The use of bindings vs bindings' is subtle. We want to use bindings for
     * converting the value of the variables (so that shadowing works properly,
     * and we can reference old bindings), but we want to use bindings' when
     * talking about the body. *)
    let little_bang_ast_expr_assignment_converted =
      a_translate_expr
        bindings
        little_bang_ast_expr_assignment
        destination
    in
    let Tiny_bang_ast.Expr (_, little_bang_ast_expr_body_clauses) =
      a_translate_expr bindings' little_bang_ast_expr_body binding_ident
    in
    append_to_clause_list
      little_bang_ast_expr_assignment_converted
      little_bang_ast_expr_body_clauses
  | Little_bang_ast.Var_expr (
      _, Little_bang_ast.Var (_, little_bang_ast_expr_var_ident)
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
              a_translate_ident bindings little_bang_ast_expr_var_ident,
              None
            )
          )
        )
      ]
    )

and a_translate_value bindings little_bang_ast_value =
  match little_bang_ast_value with
  | Little_bang_ast.Int_value (_, value) ->
    Tiny_bang_ast.Int_value (Tiny_bang_ast_uid.next_uid (), value)
  | Little_bang_ast.Empty_onion (_) ->
    Tiny_bang_ast.Empty_onion_value (Tiny_bang_ast_uid.next_uid ())
  | Little_bang_ast.Function (_, little_bang_ast_pattern, little_bang_ast_body) ->
    begin
        let (pat, bindings') =
            a_translate_pattern bindings little_bang_ast_pattern (Tiny_bang_ast.new_fresh_ident ())
        in
        Tiny_bang_ast.Function_value (
          (Tiny_bang_ast_uid.next_uid ()),
          pat,
          a_translate_expr bindings' little_bang_ast_body (Tiny_bang_ast.new_fresh_ident ())
        )
    end

and a_translate_primitive_pattern bindings binding_ident underlying filter =
  match underlying with
  | Little_bang_ast.Var_pattern (_, Little_bang_ast.Var (_, lb_ident)) ->
    begin
      let translated = derive_fresh_ident lb_ident in
      let bindings' = Ident_map.add lb_ident translated bindings in
      (Tiny_bang_ast.Pattern (
        (Tiny_bang_ast_uid.next_uid ()),
        Tiny_bang_ast.Pvar (
          (Tiny_bang_ast_uid.next_uid ()),
          binding_ident
        ),
        (Tiny_bang_ast.Pvar_map.singleton
          (Tiny_bang_ast.Pvar (Tiny_bang_ast_uid.next_uid (), binding_ident))
          (filter @@ Tiny_bang_ast.Pvar (Tiny_bang_ast_uid.next_uid (), translated))
        )
      ), bindings')
    end
  | Little_bang_ast.Empty_pattern (_) ->
      let tmp_ident = Tiny_bang_ast.new_fresh_ident ()
      in
      let pvar = Tiny_bang_ast.Pvar (Tiny_bang_ast_uid.next_uid (), tmp_ident)
      in
      (Tiny_bang_ast.Pattern (
        (Tiny_bang_ast_uid.next_uid ()),
        Tiny_bang_ast.Pvar (
          (Tiny_bang_ast_uid.next_uid ()),
          binding_ident
        ),
        (Tiny_bang_ast.Pvar_map.add pvar
          (Tiny_bang_ast.Empty_filter (Tiny_bang_ast_uid.next_uid ()))
          @@ Tiny_bang_ast.Pvar_map.singleton
          (Tiny_bang_ast.Pvar (Tiny_bang_ast_uid.next_uid (), binding_ident))
          (filter pvar)
        )
      ), bindings)
  | _ -> raise @@ Failure "Only Empty_pattern and Var_pattern can occur in a ref cell"

and a_translate_pattern bindings little_bang_ast_pattern binding_ident =
  match little_bang_ast_pattern with
  | Little_bang_ast.Empty_pattern (_) ->
    (Tiny_bang_ast.Pattern (
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
    ), bindings)
  | Little_bang_ast.Int_pattern (_, underlying) ->
      a_translate_primitive_pattern bindings binding_ident underlying
        (fun var -> Tiny_bang_ast.Int_filter (Tiny_bang_ast_uid.next_uid (), var))
  | Little_bang_ast.Array_pattern (_, underlying) ->
      a_translate_primitive_pattern bindings binding_ident underlying
        (fun var -> Tiny_bang_ast.Array_filter (Tiny_bang_ast_uid.next_uid (), var))
  | Little_bang_ast.Ref_pattern (_, underlying) ->
      a_translate_primitive_pattern bindings binding_ident underlying
        (fun var -> Tiny_bang_ast.Ref_filter (Tiny_bang_ast_uid.next_uid (), var))
  | Little_bang_ast.Label_pattern (_, little_bang_ast_label, little_bang_ast_subpattern) ->
    let label_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let (Tiny_bang_ast.Pattern (
        _,
        little_bang_ast_labeled_var,
        little_bang_ast_pattern_filter_rules
      ), bindings')
      =
      a_translate_pattern bindings little_bang_ast_subpattern label_ident
    in
    (Tiny_bang_ast.Pattern (
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
    ), bindings')
  | Little_bang_ast.Conjunction_pattern (_, little_bang_ast_left_subpattern, little_bang_ast_right_subpattern) ->
    let left_subexpression_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let right_subexpression_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let
      (
        Tiny_bang_ast.Pattern (
          _,
          little_bang_ast_left_labeled_var,
          little_bang_ast_left_pattern_filter_rules
        ),
        bindings'
      ) = a_translate_pattern bindings little_bang_ast_left_subpattern left_subexpression_ident in
    let (
        Tiny_bang_ast.Pattern (
          _,
          little_bang_ast_right_labeled_var,
          little_bang_ast_right_pattern_filter_rules
        ),
        bindings''
      )
      = a_translate_pattern bindings' little_bang_ast_right_subpattern right_subexpression_ident in
    (Tiny_bang_ast.Pattern (
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
    ), bindings'')
  | Little_bang_ast.Var_pattern (_, Little_bang_ast.Var (_, little_bang_ast_var_ident)) ->
    let translated = derive_fresh_ident little_bang_ast_var_ident in
    let spurious_ident = (Tiny_bang_ast.new_fresh_ident ()) in
    let bindings' = Ident_map.add little_bang_ast_var_ident translated bindings in
    (
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
                  translated
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
                      translated
                    )
                  )
                  (Tiny_bang_ast.Empty_filter (Tiny_bang_ast_uid.next_uid ()))
                  Tiny_bang_ast.Pvar_map.empty
              )
          )
      )
    ),
        bindings'
    )
;;

let a_translate little_bang_ast_expr =
  a_translate_expr Ident_map.empty little_bang_ast_expr (Tiny_bang_ast.new_fresh_ident ())
;;
