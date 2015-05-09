open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_ast_pretty;;
open Tiny_bang_ast_uid;;
open Tiny_bang_interpreter_types;;

let pretty_env env =
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
  (* Handle Not_found in a more graceful manner? Custom exception? *)
  Environment.find env x

let bound_vars_of_expr (Expr(_, cls)) =
  cls
  |> List.map (fun (Clause(_, x, _)) -> x)
  |> VarSet.of_list
;;

let rec compatibility env first_x_arg pat : value VarMap.t option =
  let (Pattern(_, first_x_pat, pfcs)) = pat in
  let rec compat x_arg x_pat : value VarMap.t option =
    (* TODO: deal with the Not_found here more gracefully? *)
    let pf = VarMap.find x_pat pfcs in
    match pf with
    (* Starting with conjunction, since we need to address that first. *)
    | Conjunction_filter(_, x_pat_1, x_pat_2) ->
        begin
          match (compat x_arg x_pat_1, compat x_arg x_pat_2) with
          | (Some cl1, Some cl2) ->
              let m = VarMap.fold
                  (fun k -> fun v -> fun m ->
                                VarMap.add k v m)
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
              Some VarMap.empty
          | Label_value(_, l, x), Label_filter(_, l', x') ->
              if l = l' then compat x x' else None
          | _, _ ->
              None
        in
        (* Adds the binding for this point in the pattern. If this is      *)
				(* premature (because, for instance, the next level up is an       *)
				(* onion), the fact that this occurs postfix will erase this       *)
				(* binding for the appropriate one.                                *)
        Option.map (VarMap.add x_pat v) lower_map
  in
  compat first_x_arg first_x_pat
;;

let rec application_match env x_fn x_arg : clause list option =
  print_string ("Environment: " ^ pretty_env env ^ "\n");
  print_string ("Lookup: " ^ pretty_var x_fn ^ "\n");
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
              |> VarMap.enum
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

and var_replace_value fn v =
  match v with
  | Empty_onion_value(_) -> v
  | Label_value(_, l, x) -> Label_value(next_uid(), l, fn x)
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
    |> VarSet.of_list 
  in
  let repl_fn (Var(_, i, fso) as x) =
    if VarSet.mem x bound_variables
      then Var(next_uid(), i, Some freshening_stack)
      else x
  in
  (* Now freshen the results of application matching. *)
  List.map (var_replace_clause repl_fn) cls
;;

let rec evaluate env lastvar cls =
  print_string (
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
;;

let eval (Expr(_, cls)) =
  let env = Environment.create(20) in
  let cls' = var_freshen (Freshening_stack []) cls in
  evaluate env None cls'
;;
