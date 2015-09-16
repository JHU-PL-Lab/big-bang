(** Functions that abstract common operations when translating Big Bang into
    Little Bang. *)

open Batteries;;


(**
 * Given an input expression, surround it with the prelude. *)
let (apply_prelude : Little_bang_ast.expr -> Little_bang_ast.expr) = begin
  let open Little_bang_ast in
  let prelude_path = "./src/big-bang-translator/prelude.lb" (*TODO: use Oasis to fill this in*)
  in
  let f = open_in_bin prelude_path
  in
  let unprocessed = fst @@ Little_bang_parser.parse_little_bang_program f
  in
  close_in f;
  let rec update_expr expr replacement =
    match expr with
    | Var_expr (_, Var(_, Ident("BIG_BANG_COMPILATION_UNIT"))) -> replacement
    | Value_expr (_, Function(_, pat, body)) ->
        Value_expr (
          Tiny_bang_ast_uid.next_uid (),
          Function(
            Tiny_bang_ast_uid.next_uid (),
            pat,
            update_expr body replacement
          )
        )
    | Label_expr (_, label, expr') ->
        Label_expr (Tiny_bang_ast_uid.next_uid (), label, update_expr expr' replacement)
    | Onion_expr (_, expr1, expr2) ->
        Onion_expr (Tiny_bang_ast_uid.next_uid (), update_expr expr1 replacement, update_expr expr2 replacement)
    | Let_expr (_, name, expr1, expr2) ->
        Let_expr (
          Tiny_bang_ast_uid.next_uid (),
          name,
          update_expr expr1 replacement,
          update_expr expr2 replacement
        )
    | Appl_expr (_, expr1, expr2) ->
        Appl_expr (Tiny_bang_ast_uid.next_uid (), update_expr expr1 replacement, update_expr expr2 replacement)
    | Builtin_expr (_, op, args) ->
        Builtin_expr (
          Tiny_bang_ast_uid.next_uid (),
          op,
          List.map (fun x -> update_expr x replacement) args
        )
    | Ref_expr (_, expr') ->
        Ref_expr (Tiny_bang_ast_uid.next_uid (), update_expr expr' replacement)
    | x -> x
  in
  update_expr unprocessed
end
;;

let little_bang_var_expr identifier =
  Little_bang_ast.Var_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Var (
          Tiny_bang_ast_uid.next_uid (),
          identifier
        )
      )
;;

(** Create an onion out of the given expressions. *)
let fold_onion (expressions : Little_bang_ast.expr list) : Little_bang_ast.expr =
  match expressions with
  | [] ->
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )
  | _ ->
    expressions
    |> List.reduce (
      fun left_expression right_expression ->
        Little_bang_ast.Onion_expr (
          Tiny_bang_ast_uid.next_uid (),
          left_expression,
          right_expression
        )
    )
;;

(** Create an onion out of the given expressions. *)
let fold_big_bang_onion
    (expressions : Big_bang_ast.expression list)
  : Big_bang_ast.expression =
  match expressions with
  | [] ->
    Big_bang_ast.Expression_literal (
      Tiny_bang_ast_uid.next_uid (),
      Big_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )
  | _ ->
    expressions
    |> List.reduce (
      fun left_expression right_expression ->
        Big_bang_ast.Expression_operation (
          Tiny_bang_ast_uid.next_uid (),
          Big_bang_ast.Operation_onioning (
            Tiny_bang_ast_uid.next_uid (),
            left_expression,
            right_expression
          )
        )
    )
;;

(** Create a conjunction out of the given patterns. *)
let fold_conjunction (patterns : Little_bang_ast.pattern list) : Little_bang_ast.pattern =
  match patterns with
  | [] -> Little_bang_ast.Empty_pattern (
      Tiny_bang_ast_uid.next_uid ()
    )
  | _ ->
    patterns
    |> List.reduce (
      fun left_pattern right_pattern ->
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          left_pattern,
          right_pattern
        )
    )
;;

(** Wrap a Little Bang function with a guard to prevent extra parameters (Section 8.5.3
    Extra Arguments handling). *)
let prevent_extra_parameters
    (quantity_of_parameters : int)
    (little_bang_function_expression : Little_bang_ast.expr)
  : Little_bang_ast.expr =
  Little_bang_ast.Onion_expr (
    Tiny_bang_ast_uid.next_uid (),
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Label_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Tiny_bang_ast.Label (
            Tiny_bang_ast.Ident (string_of_int quantity_of_parameters)
          ),
          Little_bang_ast.Empty_pattern (
            Tiny_bang_ast_uid.next_uid ()
          )
        ),
        Little_bang_ast.Appl_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Value_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Empty_onion (
              Tiny_bang_ast_uid.next_uid ()
            )
          ),
          Little_bang_ast.Value_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Empty_onion (
              Tiny_bang_ast_uid.next_uid ()
            )
          )
        )
      )
    ),
    little_bang_function_expression
  )
;;

(* Encoding 8.9 *)
let seal
    (little_bang_expression : Little_bang_ast.expr)
  : Little_bang_ast.expr =
  let fixpoint_identifier = Little_bang_ast.new_fresh_ident () in
  let f_identifier = Little_bang_ast.new_fresh_ident () in
  let g_identifier = Little_bang_ast.new_fresh_ident () in
  let h_identifier = Little_bang_ast.new_fresh_ident () in
  let x_identifier = Little_bang_ast.new_fresh_ident () in
  let y_identifier = Little_bang_ast.new_fresh_ident () in

  let seal_identifier = Little_bang_ast.new_fresh_ident () in
  let object_identifier = Little_bang_ast.new_fresh_ident () in
  let message_identifier = Little_bang_ast.new_fresh_ident () in

  Little_bang_ast.Let_expr (
    Tiny_bang_ast_uid.next_uid (),
    Little_bang_ast.Var (
      Tiny_bang_ast_uid.next_uid (),
      fixpoint_identifier
    ),
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Var_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Var (
            Tiny_bang_ast_uid.next_uid (),
            f_identifier
          )
        ),
        Little_bang_ast.Appl_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Value_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Function (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var_pattern (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Var (
                  Tiny_bang_ast_uid.next_uid (),
                  g_identifier
                )
              ),
              Little_bang_ast.Value_expr (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Function (
                  Tiny_bang_ast_uid.next_uid (),
                  Little_bang_ast.Var_pattern (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Var (
                      Tiny_bang_ast_uid.next_uid (),
                      x_identifier
                    )
                  ),
                  Little_bang_ast.Appl_expr (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Appl_expr (
                      Tiny_bang_ast_uid.next_uid (),
                      Little_bang_ast.Var_expr (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var (
                          Tiny_bang_ast_uid.next_uid (),
                          g_identifier
                        )
                      ),
                      Little_bang_ast.Var_expr (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var (
                          Tiny_bang_ast_uid.next_uid (),
                          g_identifier
                        )
                      )
                    ),
                    Little_bang_ast.Var_expr (
                      Tiny_bang_ast_uid.next_uid (),
                      Little_bang_ast.Var (
                        Tiny_bang_ast_uid.next_uid (),
                        x_identifier
                      )
                    )
                  )
                )
              )
            )
          ),
          Little_bang_ast.Value_expr (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Function (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var_pattern (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Var (
                  Tiny_bang_ast_uid.next_uid (),
                  h_identifier
                )
              ),
              Little_bang_ast.Value_expr (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Function (
                  Tiny_bang_ast_uid.next_uid (),
                  Little_bang_ast.Var_pattern (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Var (
                      Tiny_bang_ast_uid.next_uid (),
                      y_identifier
                    )
                  ),
                  Little_bang_ast.Appl_expr (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Appl_expr (
                      Tiny_bang_ast_uid.next_uid (),
                      Little_bang_ast.Var_expr (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var (
                          Tiny_bang_ast_uid.next_uid (),
                          f_identifier
                        )
                      ),
                      Little_bang_ast.Appl_expr (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var_expr (
                          Tiny_bang_ast_uid.next_uid (),
                          Little_bang_ast.Var (
                            Tiny_bang_ast_uid.next_uid (),
                            h_identifier
                          )
                        ),
                        Little_bang_ast.Var_expr (
                          Tiny_bang_ast_uid.next_uid (),
                          Little_bang_ast.Var (
                            Tiny_bang_ast_uid.next_uid (),
                            h_identifier
                          )
                        )
                      )
                    ),
                    Little_bang_ast.Var_expr (
                      Tiny_bang_ast_uid.next_uid (),
                      Little_bang_ast.Var (
                        Tiny_bang_ast_uid.next_uid (),
                        y_identifier
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Little_bang_ast.Let_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Var (
        Tiny_bang_ast_uid.next_uid (),
        seal_identifier
      ),
      Little_bang_ast.Appl_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Var_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Var (
            Tiny_bang_ast_uid.next_uid (),
            fixpoint_identifier
          )
        ),
        Little_bang_ast.Value_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Function (
            Tiny_bang_ast_uid.next_uid (),
            Little_bang_ast.Var_pattern (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Var (
                Tiny_bang_ast_uid.next_uid (),
                seal_identifier
              )
            ),
            Little_bang_ast.Value_expr (
              Tiny_bang_ast_uid.next_uid (),
              Little_bang_ast.Function (
                Tiny_bang_ast_uid.next_uid (),
                Little_bang_ast.Var_pattern (
                  Tiny_bang_ast_uid.next_uid (),
                  Little_bang_ast.Var (
                    Tiny_bang_ast_uid.next_uid (),
                    object_identifier
                  )
                ),
                Little_bang_ast.Onion_expr (
                  Tiny_bang_ast_uid.next_uid (),
                  Little_bang_ast.Value_expr (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Function (
                      Tiny_bang_ast_uid.next_uid (),
                      Little_bang_ast.Var_pattern (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var (
                          Tiny_bang_ast_uid.next_uid (),
                          message_identifier
                        )
                      ),
                      Little_bang_ast.Appl_expr (
                        Tiny_bang_ast_uid.next_uid (),
                        Little_bang_ast.Var_expr (
                          Tiny_bang_ast_uid.next_uid (),
                          Little_bang_ast.Var (
                            Tiny_bang_ast_uid.next_uid (),
                            object_identifier
                          )
                        ),
                        Little_bang_ast.Onion_expr (
                          Tiny_bang_ast_uid.next_uid (),
                          Little_bang_ast.Var_expr (
                            Tiny_bang_ast_uid.next_uid (),
                            Little_bang_ast.Var (
                              Tiny_bang_ast_uid.next_uid (),
                              message_identifier
                            )
                          ),
                          Little_bang_ast.Label_expr (
                            Tiny_bang_ast_uid.next_uid (),
                            Tiny_bang_ast.Label (
                              Tiny_bang_ast.Ident "self"
                            ),
                            Little_bang_ast.Appl_expr (
                              Tiny_bang_ast_uid.next_uid (),
                              Little_bang_ast.Var_expr (
                                Tiny_bang_ast_uid.next_uid (),
                                Little_bang_ast.Var (
                                  Tiny_bang_ast_uid.next_uid (),
                                  seal_identifier
                                )
                              ),
                              Little_bang_ast.Var_expr (
                                Tiny_bang_ast_uid.next_uid (),
                                Little_bang_ast.Var (
                                  Tiny_bang_ast_uid.next_uid (),
                                  object_identifier
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  Little_bang_ast.Var_expr (
                    Tiny_bang_ast_uid.next_uid (),
                    Little_bang_ast.Var (
                      Tiny_bang_ast_uid.next_uid (),
                      object_identifier
                    )
                  )
                )
              )
            )
          )
        )
      ),
      Little_bang_ast.Appl_expr (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Var_expr (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Var (
            Tiny_bang_ast_uid.next_uid (),
            seal_identifier
          )
        ),
        little_bang_expression
      )
    )
  )
;;

(*
Z = \f.(\x.f (\v.((x x) v))) (\x.f (\v.((x x) v)))

Reference: https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator
*)
let y_combinator
    (expression : Big_bang_ast.expression)
  : Big_bang_ast.expression =
  let f_identifier = Big_bang_ast_utils.new_fresh_identifier () in
  let x_identifier = Big_bang_ast_utils.new_fresh_identifier () in
  let v_identifier = Big_bang_ast_utils.new_fresh_identifier () in
  let symmetric_part () =
    Big_bang_ast.Expression_literal (
      Tiny_bang_ast_uid.next_uid (),
      Big_bang_ast.Anonymous_function (
        Tiny_bang_ast_uid.next_uid (),
        [x_identifier],
        Big_bang_ast.Block (
          Tiny_bang_ast_uid.next_uid (),
          [
            Big_bang_ast.Expression_application_or_indexing (
              Tiny_bang_ast_uid.next_uid (),
              Big_bang_ast.Expression_identifier (
                Tiny_bang_ast_uid.next_uid (),
                f_identifier
              ),
              [
                Big_bang_ast.Expression_literal (
                  Tiny_bang_ast_uid.next_uid (),
                  Big_bang_ast.Anonymous_function (
                    Tiny_bang_ast_uid.next_uid (),
                    [v_identifier],
                    Big_bang_ast.Block (
                      Tiny_bang_ast_uid.next_uid (),
                      [
                        Big_bang_ast.Expression_application_or_indexing (
                          Tiny_bang_ast_uid.next_uid (),
                          Big_bang_ast.Expression_application_or_indexing (
                            Tiny_bang_ast_uid.next_uid (),
                            Big_bang_ast.Expression_identifier (
                              Tiny_bang_ast_uid.next_uid (),
                              x_identifier
                            ),
                            [
                              Big_bang_ast.Expression_identifier (
                                Tiny_bang_ast_uid.next_uid (),
                                x_identifier
                              )
                            ]
                          ),
                          [
                            Big_bang_ast.Expression_identifier (
                              Tiny_bang_ast_uid.next_uid (),
                              v_identifier
                            )
                          ]
                        )
                      ]
                    )
                  )
                )
              ]
            )
          ]
        )
      )
    )
  in
  Big_bang_ast.Expression_application_or_indexing (
    Tiny_bang_ast_uid.next_uid (),
    Big_bang_ast.Expression_literal (
      Tiny_bang_ast_uid.next_uid (),
      Big_bang_ast.Anonymous_function (
        Tiny_bang_ast_uid.next_uid (),
        [f_identifier],
        Big_bang_ast.Block (
          Tiny_bang_ast_uid.next_uid (),
          [
            Big_bang_ast.Expression_application_or_indexing (
              Tiny_bang_ast_uid.next_uid (),
              symmetric_part (),
              [
                symmetric_part ()
              ]
            )
          ]
        )
      )
    ),
    [
      expression
    ]
  )
;;
