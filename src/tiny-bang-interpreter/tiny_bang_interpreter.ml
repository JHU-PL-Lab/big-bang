open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_ast_pretty;;
open Tiny_bang_ast_uid;;

let logger = Tiny_bang_logger.make_logger "Tiny_bang_interpreter";;

module Environment = Var_hashtbl;;

let pretty_env (env : value Environment.t) =
  let inner =
    env
    |> Environment.enum
    |> Enum.map (fun (x,v) -> pretty_var x ^ " = " ^ pretty_value v)
    |> Enum.fold
      (fun acc -> fun s -> if acc = "" then s else acc ^ ", " ^ s) ""
  in
  "{ " ^ inner ^ " }"
;;

let lookup env x =
  (* TODO: Handle Not_found in a more graceful manner? Custom exception? *)
  Environment.find env x
;;

let update env x v =
  Environment.replace env x v
;;

let bound_vars_of_expr (Expr(_, cls)) =
  cls
  |> List.map (fun (Clause(_, x, _)) -> x)
  |> Var_set.of_list
;;

(**
   Evaluates value compatibility for the provided arguments.  Only one argument
   and one pattern are provided; this is because, in practice, it is simpler and
   easier to check patterns one at a time.  Although the formal specification
   dictates that these patterns be checked simultaneously, Lemma 4.13 of
   Building a Typed Scripting Language justifies why the two approaches are the
   same on the value level.
   @param env The environment in which to perform the computation.
   @param first_x_arg The argument variable.
   @param pat The pattern to check for compatibility with the argument.
   @return A mapping from variables to values describing the bindings of this
          compatibility or None if compatibility does not hold.
*)
let compatibility env first_x_arg pat : value Var_map.t option =
  let (Pattern(_, first_x_pat, pfcs)) = pat in
  let rec compat x_arg x_pat : value Var_map.t option =
    (* TODO: deal with the Not_found here more gracefully? *)
    let pf = Pvar_map.find x_pat pfcs in
    match pf with
    (* Starting with conjunction, since we need to address that first. *)
    | Conjunction_filter(_, x_pat_1, x_pat_2) ->
      begin
        match (compat x_arg x_pat_1, compat x_arg x_pat_2) with
        | (Some cl1, Some cl2) ->
          let m = Var_map.fold
              (fun k -> fun v -> fun m ->
                 Var_map.add k v m)
              cl1 cl2 in
          Some m
        | _ ->
          None
      end
    | _ ->
      (* Otherwise, we need to check each value and make sure it matches the *)
      (* pattern. The ordering of the match below is important.              *)
      let v = lookup env x_arg in
      let lower_map =
        match v, pf with
        | Onion_value(_, x_arg_1, x_arg_2), _ ->
          (* Split onion values without looking at the pattern, expecting  *)
          (* one of the two sides to match. (This is sane because the      *)
          (* pattern isn't a conjunction.                                  *)
          Option.map_default Option.some
            (compat x_arg_1 x_pat) (compat x_arg_2 x_pat)
        | _, Empty_filter(_) ->
          (* Everything matches an empty filter. *)
          Some Var_map.empty
        | Label_value(_, l, x), Label_filter(_, l', x') ->
          if l = l' then compat x x' else None
        | Int_value(_, _) as i, Int_filter(_, x) ->
          Some (Var_map.singleton (var_of_pvar x) i)
        | Array_value(_, _) as i, Array_filter(_, x) ->
          Some (Var_map.singleton (var_of_pvar x) i)
        | Ref_value(_,x), Ref_filter(_, x') ->
          let x_value = lookup env x in
          Some (Var_map.singleton (var_of_pvar x') x_value)
        | _, _ ->
          None
      in
      (* Adds the binding for this point in the pattern. If this is      *)
      (* premature (because, for instance, the next level up is an       *)
      (* onion), the fact that this occurs postfix will erase this       *)
      (* binding for the appropriate one.                                *)
      Option.map (Var_map.add (var_of_pvar x_pat) v) lower_map
  in
  compat first_x_arg first_x_pat
;;

let rec application_match env x_fn x_arg : clause list option =
  logger `debug ("Environment: " ^ pretty_env env ^ "\n");
  logger `debug ("Lookup: " ^ pretty_var x_fn ^ "\n");
  match lookup env x_fn with
  | Onion_value(_, x_left, x_right) ->
    Option.map_default Option.some
      (application_match env x_left x_arg)
      (application_match env x_right x_arg)
  | Function_value(_, pat, Expr(_, cls)) ->
    begin
      let answer = compatibility env x_arg pat in
      match answer with
      | None -> None
      | Some bindings ->
        (* Turn the bindings into generated clauses. *)
        let binding_clauses =
          bindings
          |> Var_map.enum
          |> Enum.map
            (fun (x, v) ->
               let ruid = next_uid() in
               let cuid = next_uid() in
               Clause(cuid, x, Value_redex(ruid, v)))
          |> List.of_enum
        in
        Some (binding_clauses @ cls)
    end
  | _ ->
    None
;;

let rec var_replace_expr fn (Expr(_, cls)) =
  Expr(next_uid(), List.map (var_replace_clause fn) cls)

and var_replace_clause fn (Clause(_, x, r)) =
  Clause(next_uid(), fn x, var_replace_redex fn r)

and var_replace_redex fn r =
  match r with
  | Value_redex(_, v) -> Value_redex(next_uid(), var_replace_value fn v)
  | Var_redex(_, x) -> Var_redex(next_uid(), fn x)
  | Appl_redex(_, x1, x2) -> Appl_redex(next_uid(), fn x1, fn x2)
  | Builtin_redex(_,o,x) -> Builtin_redex(next_uid(), o, List.map fn x)

and var_replace_value fn v =
  match v with
  | Empty_onion_value(_) -> v
  | Int_value(_, _) -> v
  | Label_value(_, l, x) -> Label_value(next_uid(), l, fn x)
  | Ref_value(_, x) -> Ref_value(next_uid(), fn x)
  | Array_value(_, x) -> Array_value(next_uid(), Array.map fn x)
  | Onion_value(_, x1, x2) -> Onion_value(next_uid(), fn x1, fn x2)
  | Function_value(_, p, e) ->
    Function_value(next_uid(), p, var_replace_expr fn e)

exception Evaluation_failure of string;;

let freshening_stack_from_var x =
  let Var(_, appl_i, appl_fso) = x in
  (* The freshening stack of a call site at top level is always
     present. *)
  let Freshening_stack idents = Option.get appl_fso in
  Freshening_stack (appl_i :: idents)
;;

let var_freshen freshening_stack cls =
  (* Build the variable freshening function. *)
  let bound_variables =
    cls
    |> List.map (fun (Clause(_, x, _)) -> x)
    |> Var_set.of_list 
  in
  let repl_fn (Var(_, i, _) as x) =
    if Var_set.mem x bound_variables
    then Var(next_uid(), i, Some freshening_stack)
    else x
  in
  (* Now freshen the results of application matching. *)
  List.map (var_replace_clause repl_fn) cls
;;

let rec var_project projector env var =
  let v = lookup env var in
  match v with
  | Empty_onion_value(_) -> None
  | Onion_value(_, x1, x2) ->
    begin
      match var_project projector env x1 with
      | Some(v') -> Some(v')
      | None -> var_project projector env x2
    end
  | _ -> projector v
;;


let project_value v = Some v ;;

let project_array v =
  match v with
  | Array_value(_, arr) -> Some arr
  | _ -> None
;;

let project_int v =
  match v with
  | Int_value(_,n) -> Some n
  | _ -> None
;;

let project_ref_var v =
  match v with
  | Ref_value(_,x) -> Some x
  | _ -> None
;;

let builtin_int_binary_op env op list_var func =
  begin
      match list_var with
      | [x1;x2] ->
        let result =
          let open Option in
          let%bind v1 = var_project project_int env x1 in
          let%bind v2 = var_project project_int env x2 in
          Some(func v1 v2)
        in
        begin
          match result with
          | Some(n) -> ([],Int_value(next_uid(), n))
          | None -> raise @@ Evaluation_failure(pretty_builtin_op op ^ " defined only on ints.")
        end
      | _ ->
        raise @@ Evaluation_failure(pretty_builtin_op op ^ " requires exactly two arguments.")
    end

let builtin_int_comparison_op env op list_var func =
  begin
      match list_var with
      | [x1;x2] ->
        let result =
          let open Option.Monad in
          let%bind v1 = var_project project_int env x1 in
          let%bind v2 = var_project project_int env x2 in
          let empty_onion_var =
            Var(next_uid(),
              Builtin_ident(Op_int_equal,0),
              Some empty_freshening_stack)
          in
          let lname = Ident(if func v1 v2 then "True" else "False") in
          let v = Label_value(next_uid(),Label lname,empty_onion_var) in
          return ([(empty_onion_var, Empty_onion_value(next_uid()))],v)
        in
        begin
          match result with
          | Some x -> x
          | None -> raise @@ Evaluation_failure(pretty_builtin_op op ^ " defined only on ints.")
        end
      | _ ->
        raise @@ Evaluation_failure(pretty_builtin_op op ^ " requires exactly two arguments.")
    end


(*metatheoretic functions for the builtin operators *)
let builtin_op env op list_var =
  match op with
  | Op_int_plus ->
    builtin_int_binary_op env op list_var (+)
  | Op_int_minus ->
    builtin_int_binary_op env op list_var (-)
  | Op_int_times ->
    builtin_int_binary_op env op list_var ( * ) (* The spaces ensure that it's not parsed as a comment *)
  | Op_int_equal ->
    builtin_int_comparison_op env op list_var (=)
  | Op_int_lessthan ->
    builtin_int_comparison_op env op list_var (<)
  | Op_array_new ->
      begin
        match list_var with
        | [x;y] ->
          begin
            match var_project project_int env x with
            | None -> raise @@ Evaluation_failure "array_new's first argument must be an int"
            | Some length ->
                begin
                  match var_project project_value env y with
                  | None -> raise @@ Evaluation_failure "array_new's second argument must be a value"
                  | Some initialValue ->
                    begin
                      let arr = Array.init length (fun _ -> Var (next_uid (), new_fresh_ident (), None))
                      in
                      Array.iter (fun var -> update env var initialValue) arr;
                      ([], Array_value (next_uid (), arr))
                    end
                end
          end
        | _ -> raise @@ Evaluation_failure "array_new takes 2 arguments"
      end
  | Op_array_length ->
      begin
        match list_var with
        | [x] ->
          begin
            match var_project project_array env x with
            | None -> raise @@ Evaluation_failure "array_length takes 1 array argument"
            | Some arr -> ([], Int_value (next_uid (), Array.length arr))
          end
        | _ -> raise @@ Evaluation_failure "array_length takes 1 argument"
      end
  | Op_array_get ->
      begin
        match list_var with
        | [x;y] ->
          begin
            let open Option.Monad in
            match (let%bind arr = var_project project_array env x
            in
            let%bind index = var_project project_int env y
            in
            if index < (Array.length arr) then
              let%bind value = var_project project_value env @@ Array.get arr index
              in
              Some ([], value)
            else raise @@ Evaluation_failure "Array index out of bounds"
            ) with
            | None -> raise @@ Evaluation_failure "array_get parameter types"
            | Some x -> x
          end
        |  _ -> raise @@ Evaluation_failure "array_get takes 2 arguments"
      end
  | Op_array_set ->
     begin
        match list_var with
        | [x;y;z] ->
          begin
            let open Option.Monad in
            match (let%bind arr = var_project project_array env x
            in
            let%bind index = var_project project_int env y
            in
            let%bind newValue = var_project project_value env z
            in
            if index < (Array.length arr) then
              begin
                update env (Array.get arr index) newValue;
                Some ([], Empty_onion_value (Tiny_bang_ast_uid.next_uid ()))
              end
            else raise @@ Evaluation_failure "Array index out of bounds"
            ) with
            | None -> raise @@ Evaluation_failure "array_get parameter types"
            | Some x -> x
          end
        |  _ -> raise @@ Evaluation_failure "array_set takes 2 arguments"
      end
  | Op_ref ->
    begin
      match list_var with
      | [x1;x2] ->
        begin
          match var_project project_ref_var env x1 with
          | None -> raise @@
            Evaluation_failure "First reference assignment operand must be a ref"
          | Some x_refbody ->
            let v = lookup env x2 in
            update env x_refbody v;
            ([], Empty_onion_value (next_uid()))
        end
      | _ ->
        raise @@ Evaluation_failure "For reference assignment 2 arguments are required"
    end
;;

let rec evaluate env lastvar cls =
  logger `debug (
    pretty_env env ^ "\n" ^
    (Option.default "?" (Option.map pretty_var lastvar)) ^ "\n" ^
    (cls
     |> List.map pretty_clause
     |> List.fold_left (fun acc -> fun s -> acc ^ s ^ "; ") "") ^ "\n\n");
  flush stdout;
  match cls with
  | [] ->
    begin
      match lastvar with
      | Some(x) -> (x, env)
      | None ->
        (* TODO: different exception? *)
        raise (Failure "evaluation of empty expression!")
    end
  | (Clause(_, x, r)):: t ->
    match r with
    | Value_redex(_, v) ->
      Environment.add env x v;
      evaluate env (Some x) t
    | Var_redex(_, x') ->
      let v = lookup env x' in
      Environment.add env x v;
      evaluate env (Some x) t
    | Appl_redex(_, x', x'') ->
      begin
        match application_match env x' x'' with
        | None -> raise (Evaluation_failure
                           ("failed to apply " ^ pretty_var x' ^ " to " ^ pretty_var x''))
        | Some to_inline ->
          let freshening_stack = freshening_stack_from_var x in
          let freshened_inline = var_freshen freshening_stack to_inline in
          (* And insert it into our evaluation. *)
          let Clause(_, last_var, _) = List.last freshened_inline in
          let ans_clause =
            Clause(next_uid(), x,
                   Var_redex(next_uid(), last_var)) in
          evaluate env (Some x) (freshened_inline @ (ans_clause :: t))
      end
    | Builtin_redex(_, op, list_x) ->
      let (updates,v) = builtin_op env op list_x in
      List.iter (fun (x',v') -> update env x' v') updates;
      update env x v;
      evaluate env (Some x) t
;;

let eval (Expr(_, cls)) =
  let env = Environment.create(20) in
  let cls' = var_freshen (Freshening_stack []) cls in
  evaluate env None cls'
;;
