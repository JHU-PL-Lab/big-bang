(** Values that are commonly used when translating Big Bang AST into Little Bang
    AST. Constants are functions that take unit instead of simple values because
    we never want to reuse the same `ast_uid'.

    Notes such as `Encoding <number>` are references to Zach's thesis, Building
    a Typed Scripting Language. *)

open Big_bang_translator_helpers;;

(** Encoding 8.1 *)
let little_bang_true () : Little_bang_ast.expr =
  Little_bang_ast.Label_expr (
    Tiny_bang_ast_uid.next_uid (),
    Tiny_bang_ast.Label (
      Tiny_bang_ast.Ident "True"
    ),
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )
  )
;;

(** Encoding 8.1 *)
let little_bang_false () : Little_bang_ast.expr =
  Little_bang_ast.Label_expr (
    Tiny_bang_ast_uid.next_uid (),
    Tiny_bang_ast.Label (
      Tiny_bang_ast.Ident "False"
    ),
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )
  )
;;

(** Encoding 8.1 *)
let little_bang_true_pattern () : Little_bang_ast.pattern =
  Little_bang_ast.Label_pattern (
    Tiny_bang_ast_uid.next_uid (),
    Tiny_bang_ast.Label (
      Tiny_bang_ast.Ident "True"
    ),
    Little_bang_ast.Empty_pattern (
      Tiny_bang_ast_uid.next_uid ()
    )
  )
;;

(** Encoding 8.1 *)
let little_bang_false_pattern () : Little_bang_ast.pattern =
  Little_bang_ast.Label_pattern (
    Tiny_bang_ast_uid.next_uid (),
    Tiny_bang_ast.Label (
      Tiny_bang_ast.Ident "False"
    ),
    Little_bang_ast.Empty_pattern (
      Tiny_bang_ast_uid.next_uid ()
    )
  )
;;

(** Encoding 8.2 *)
let and_truth_table () : Little_bang_ast.expr =
  fold_onion [
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_false ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_false ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_false ()
      )
    )
  ]
;;

(** Encoding 8.2 *)
let or_truth_table () : Little_bang_ast.expr =
  fold_onion [
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_false ()
      )
    )
  ]
;;

(** Based on Encoding 8.2 *)
let xor_truth_table () : Little_bang_ast.expr =
  fold_onion [
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_false ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_true_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_true_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_true ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        Little_bang_ast.Conjunction_pattern (
          Tiny_bang_ast_uid.next_uid (),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "1"
            ),
            little_bang_false_pattern ()
          ),
          Little_bang_ast.Label_pattern (
            Tiny_bang_ast_uid.next_uid (),
            Tiny_bang_ast.Label (
              Tiny_bang_ast.Ident "2"
            ),
            little_bang_false_pattern ()
          )
        ),
        little_bang_false ()
      )
    )
  ]
;;

(** Encoding 8.2 *)
let not_truth_table () : Little_bang_ast.expr =
  fold_onion [
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        little_bang_true_pattern (),
        little_bang_false ()
      )
    );
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Function (
        Tiny_bang_ast_uid.next_uid (),
        little_bang_false_pattern (),
        little_bang_true ()
      )
    )
  ]
;;

(* Returns an expression that causes a runtime error (not a type error).

   It does so by looping forever, because Big Bang doesn't have a proper error
   mechanism. Therefore, this is extremely annoying for the users, use with
   caution.

   (fun (f) = f f end)(fun (f) = f f end) # provoke infinite loop
 *)
let runtime_error () : Big_bang_ast.expression =
  let f_identifier = Big_bang_ast_utils.new_fresh_identifier () in
  let part () =
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
              Big_bang_ast.Expression_identifier (
                Tiny_bang_ast_uid.next_uid (),
                f_identifier
              ),
              [
                Big_bang_ast.Expression_identifier (
                  Tiny_bang_ast_uid.next_uid (),
                  f_identifier
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
    part (),
    [
      part ()
    ]
  )
;;

let void () : Little_bang_ast.expr =
  Little_bang_ast.Label_expr (
    Tiny_bang_ast_uid.next_uid (),
    Tiny_bang_ast.Label (
      Tiny_bang_ast.Ident "Void"
    ),
    Little_bang_ast.Value_expr (
      Tiny_bang_ast_uid.next_uid (),
      Little_bang_ast.Empty_onion (
        Tiny_bang_ast_uid.next_uid ()
      )
    )
  )
;;
