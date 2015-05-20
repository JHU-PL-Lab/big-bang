(**
  This module contains the logic necessary to construct proofs of type
  compatibility.
  
  This computation is significantly more complicated than value compatibility
  due to the lack of a property similar to Lemma 4.13 of Building a Typed 
  Scripting Language: it is insufficient to check each pattern independently due
  to the fact that there may be multiple lower bounds for each variable.
  Because multiple lower bounds may exist for each type variable (in comparison
  to the single values of variables at runtime), multiple compatibility proofs
  may exist for each query.  Further, compatibility must handle cycles in
  pattern matching through occurrence checking.
 *)

open Batteries;;
open Printf;;

open Tiny_bang_ast;;
open Tiny_bang_compatibility_types;;
open Tiny_bang_string_utils;;
open Tiny_bang_types;;
open Tiny_bang_types_pretty;;
open Tiny_bang_utils;;

(* ************************************************************************** *)
(* LOGGER *)

let logger = Tiny_bang_logger.make_logger "Tiny_bang_compatibility";;

(* ************************************************************************** *)
(* UTILITIES *)
(* This first section contains a number of utilities and types used throughout
   the rest of the module. *)
  
(** A data type describing the expectations of a compatibility task. *)
type compatibility_task_expectation =
  | Must_match (** Stop checking if the task does not match. *)
  | Must_not_match (** Stop checking if the task matches. *)
  | Must_report (** Report whether the task matches. *)
;;

(** This data structure describes a task to be performed during a compatibility
    search. *)
type compatibility_task =
  { ct_pat : pattern_type  (** the pattern being matched *)
  ; ct_binding : bool      (** whether bindings should be created *)
  ; ct_expect_match : compatibility_task_expectation  (** whether this value is
                                                          expected to match *)
  };;

(** Checks an answer with a compatibility expectation.  Returns [true] if the
    provided answer is acceptable given the expectation and [false] if it is
    not. *)
let check_expectation expectation answer =
  match expectation with
    | Must_match -> answer
    | Must_not_match -> not answer
    | Must_report -> true
;;

module Compatibility_task_hash =
struct
  type t = compatibility_task
  let hash = Hashtbl.hash
  let equal = (=)
end;;

module Compatibility_task_hashtbl = Hashtbl.Make(Compatibility_task_hash);;

module Compatibility_task_ord =
struct
  type t = compatibility_task
  let compare = compare
end;;

module Compatibility_task_set = Set.Make(Compatibility_task_ord);;

(** A convenience routine for creating a compatibility task.
    @param pat The pattern in the task.
    @param binding [true] if the pattern should bind variables; [false] if it
                   should not.
    @param expect_match A [compatibility_task_expectation] indicating how the
                        task must result.
 *)
let compatibility_task pat binding expect_match =
  { ct_pat = pat
  ; ct_binding = binding
  ; ct_expect_match = expect_match
  }
;;

type bind_state = 
  | Do_bind (** The "bind-needed" symbol in the theory: binding is pending. *)
  | Dont_bind (** The "bind-complete" symbol in the theory: vars are bound. *)
;;

type occurrence =
  { co_type_selected : filtered_type
  ; co_tasks : Compatibility_task_set.t
  }
;;

module Occurrence_ord =
struct
  type t = occurrence
  let compare = compare
end;;

module Occurrence_set = Set.Make(Occurrence_ord);;

(** The type of internal digest results.  In addition to the bindings, two
    task result lists are present.  The rightmost list contains answers for
    constructor tasks; the leftmost list contains answers for tasks which may
    include non-constructor patterns. *)
type internal_digest_result =
  | Internal_digest_result of compatibility_bindings * bool list * bool list
;;

module Internal_digest_result_ord =
struct
  type t = internal_digest_result
  let compare = compare
end;;

module Internal_digest_result_set = Set.Make(Internal_digest_result_ord);;

(**
  A structure representing generic compatibility queries.  Each such query is
  either an [Immediate_answer] or it is a [Recursive_compatibility_query] which
  TODO: document
*)
type 'a compatibility_query =
  | Recursive_compatibility_query of compatibility_task * (bool -> 'a list)
  | Immediate_answer of 'a
;;

(**
  A function used for defensive checking of the invariant that the return lists
  should be the same size as the task lists in the functions below.  This
  function only validates invariants at runtime to obtain fail-fast behavior;
  in a production build, it can be replaced with one which skips the tests and
  returns the list.
*)
let enforce_list_size task_list answer_list =
  if List.length task_list <> List.length answer_list then
    logger `warn
      (sprintf "List size %d does not match expected size %d."
        (List.length answer_list) (List.length task_list))
  ;
  answer_list;;
(* TODO: figure out a good way to get three modes at compile-time or something
         similar: exception, log warning, and no-op *)
(*
    else raise (Invariant_failure(
      sprintf "List size %d does not match expected size %d."
        (List.length answer_list) (List.length task_list)));;
*)

(* ************************************************************************** *)
(* PRETTY PRINTING *)
(* Pretty printing for the above data structures for logging purposes. *)

let pretty_bind_state bind_state =
  match bind_state with
    | Do_bind -> "Do_bind"
    | Dont_bind -> "Dont_bind"
;;

let pretty_compatibility_task_expectation expect =
  match expect with
    | Must_match -> "Must_match"
    | Must_not_match -> "Must_not_match"
    | Must_report -> "Must_report"
;;

let pretty_task task =
  sprintf
    "Task: %s %s %s"
    (pretty_compatibility_task_expectation task.ct_expect_match)
    (pretty_pattern_type task.ct_pat)
    (if task.ct_binding then " (binding)" else "")
;;

let pretty_task_list tasks =
  concat_sep_delim "[" "]" ", "
    (tasks |> List.enum |> Enum.map pretty_task)
;;

let pretty_binding (ft,a) =
  pretty_filtered_type ft ^ " <: " ^ pretty_tvar a
;;

let pretty_binding_set bindings =
  bindings
    |> List.enum
    |> Enum.map pretty_binding
    |> concat_sep_delim "{" "}" ","
;;

let answers_to_str answers =
  answers
    |> List.enum
    |> Enum.map (fun ans -> if ans then "Y" else "N")
    |> concat_sep_delim "[" "]" ","
;;

let pretty_result (Compatibility_result (bindings,answers)) =
  let answers_str = answers_to_str answers in
  let bindings_str = pretty_binding_set bindings in
  answers_str ^ "@" ^ bindings_str
;;

let pretty_result_set results =
  results
    |> Compatibility_result_set.enum
    |> Enum.map pretty_result
    |> concat_sep_delim "{" "}" ", "
;;

let pretty_internal_result (Internal_digest_result(
      bindings,answers,ctr_answers)) =
  let answers_str = answers_to_str answers in
  let ctr_answers_str = answers_to_str ctr_answers in
  let bindings_str = pretty_binding_set bindings in
  answers_str ^ "," ^ ctr_answers_str ^ "@" ^ bindings_str
;;

let pretty_internal_result_set results =
  results
    |> Internal_digest_result_set.enum
    |> Enum.map pretty_internal_result
    |> concat_sep_delim "{" "}" ", "
;;

let pretty_query pretty_result query =
  match query with
    | Recursive_compatibility_query(t,h) -> "QRec: " ^ pretty_task t
    | Immediate_answer(a) -> "QImm: " ^ pretty_result a
;;

(* ************************************************************************** *)
(* INTERNAL ROUTINES *)
(* This section contains a number of routines used internally when computing
   compatibility. *)

(* TODO: this code should raise an exception if a non-contractive type is
         encountered *)

(**
  Determines internal compatibility results for a type variable.  This function
  operates on a list of tasks in tandem.  The generated result is a set; each
  element in the set is a pair between a set of bindings and a list of answers.
  Each answer corresponds to the task at the same index in the task list and
  is a boolean indicating whether that pattern match succeeded or failed in
  that case.  (Different results from the set may have different answers for
  the same pattern.)
  @param cs The constraint set to use in checking compatibility.
  @param occurrences The set of occurrences observed so far in this computation.
  @param a The type variable for the argument.
  @param tasks The tasks to perform.
  @param bind_state The binding state of the computation.
  @return The results as described above.
 *)
let rec compatibility_by_tvar
      (cs : Constraint_database.t)
      (occurrences : Occurrence_set.t)
      (a : tvar)
      (tasks : compatibility_task list)
      (bind_state : bind_state)
      : Compatibility_result_set.t =
  Tiny_bang_logger.bracket_log logger `trace
    (sprintf
      ("compatibility_by_tvar: Checking compatibility of %s in binding state %s\n    with tasks %s")
      (pretty_tvar a)
      (pretty_bind_state bind_state)
      (pretty_task_list tasks)
    )
    pretty_result_set
  @@ fun () ->
  if List.is_empty tasks
    then
      (* LEAF RULE *)
      Compatibility_result_set.singleton
            (Compatibility_result([], []))
    else
      (* TYPE SELECTION RULE *)
      (* Step 1: Select appropriate lower bounds for this type variable. *)
      let restricted_types = Constraint_database.type_lower_bounds_of a cs in
      let results = restricted_types
        |> Enum.map
            (fun (Filtered_type(t,filts_pos,filts_neg) as ft) ->
              (* Step 2: Determine the tasks implied by each of the filter
                         sets. *)
              let pos_tasks =
                filts_pos
                  |> Pattern_type_set.enum
                  |> Enum.map (fun pat ->
                        compatibility_task pat false Must_match)
                  |> List.of_enum
              in
              let neg_tasks =
                filts_neg
                  |> Pattern_type_set.enum
                  |> Enum.map (fun pat ->
                        compatibility_task pat false Must_not_match)
                  |> List.of_enum
              in
              let all_tasks =
                List.of_enum @@ Enum.concat @@ List.enum @@ List.map List.enum
                  [pos_tasks; neg_tasks; tasks]
              in 
              (* Step 3: Describe this occurrence. *)
              let occurrence =
                { co_type_selected = ft
                ; co_tasks = Compatibility_task_set.of_list all_tasks
                }
              in
              if Occurrence_set.mem occurrence occurrences
                then
                  (* We've already seen this occurrence on this computational
                     chain.  This represents a cycle.  According to the formal
                     definition of compatibility, such cycles do not yield
                     compatibility proofs.  This is sound because types which
                     cannot be proven in any other way are empty (although this
                     might not be the case if e.g. built-in lazy values
                     existed. *)
                  Compatibility_result_set.empty
                else
                  (* Step 4: Use this concrete type to continue the analysis. *)
                  let occurrences' =
                    Occurrence_set.add occurrence occurrences
                  in
                  let results_by_type =
                    compatibility_by_type cs occurrences' t all_tasks bind_state
                  in
                  (* Step 5: Remove from the results sequences the answers to
                             those tasks which correspond to filters on the
                             selected type. *)
                  results_by_type
                    |> Compatibility_result_set.map
                          (fun (Compatibility_result(bindings,answers)) ->
                            let answers' = List.drop
                              (List.length pos_tasks + List.length neg_tasks)
                              answers
                            in Compatibility_result(bindings,answers'))
            )
      in
      Enum.fold (Compatibility_result_set.union) Compatibility_result_set.empty
        results

(**
  Determines internal compatibility results for a concrete type.  The arguments
  and return are of the same form as [compatibility_by_tvar].
  @param cs The constraint set to use in checking compatibility.
  @param occurrences The set of occurrences observed so far in this computation.
  @param typ The type for the argument.
  @param tasks The tasks to perform.
  @param bind_state The binding state of the computation.
  @return The results as described above.
 *)
and compatibility_by_type
      (cs : Constraint_database.t)
      (occurrences : Occurrence_set.t)
      (typ : tbtype)
      (tasks : compatibility_task list)
      (bind_state : bind_state)
      : Compatibility_result_set.t =
  Tiny_bang_logger.bracket_log logger `trace
    (sprintf
      "compatibility_by_type: Checking compatibility of %s in binding state %s\n    with tasks %s"
      (pretty_type typ)
      (pretty_bind_state bind_state)
      (pretty_task_list tasks)
    )
    pretty_result_set
  @@ fun () ->
  (* We start by operating on the non-constructor patterns one at a time.  After
     every non-constructor pattern has been digested, we operate on all of the
     constructor patterns in tandem.  The following invocation accomplishes
     this task; we then perform binding if necessary. *)
  let results = compatibility_by_type_maybe_with_non_constr_pats
                  cs occurrences typ tasks []
  in
  results
    |> Internal_digest_result_set.enum
    |> Enum.map
          (fun (Internal_digest_result(bindings,answers,answers_empty)) ->
            (* The last answers list should be empty; otherwise, something is
               wrong with the code. *)
            (* TODO: assert answers_empty is empty. *)
            (* Now we consider binding if it is appropriate to do so. *)
            let new_bindings = match bind_state with
              | Dont_bind ->
                  []
              | Do_bind ->
                  (* BINDING RULE *)
                  (* Since we need to do binding here, we create the appropriate
                     lower bounds for each binidng pattern variable which
                     *matched*.  (There may be binding patterns which do not
                     match here as their expectation is to report, such as when
                     we here are on the left of an onion.)  If the pattern did
                     not match, no bindings are generated. *)
                  (* Partition the patterns into sets based on their answers. *)
                  let filts_pos,filts_neg = List.fold_left2
                    (fun (filts_pos,filts_neg) -> fun task -> fun answer ->
                      let pat = task.ct_pat in
                      if answer
                        then (Pattern_type_set.add pat filts_pos, filts_neg)
                        else (filts_pos, Pattern_type_set.add pat filts_neg)
                    )
                    (Pattern_type_set.empty, Pattern_type_set.empty)
                    tasks
                    answers
                  in
                  (* Now create the bindings themselves. *)
                  List.fold_left2
                    (fun bindings -> fun task -> fun answer ->
                      if answer
                        then
                          let ft = Filtered_type(typ,filts_pos,filts_neg) in
                          let (Pattern_type(a,_)) = task.ct_pat in
                          (ft, a) :: bindings
                        else
                          bindings
                    )
                    []
                    tasks
                    answers
            in
            Compatibility_result(bindings @ new_bindings, answers)
          )
    |> Compatibility_result_set.of_enum

(**
  Breaks down non-compatibility patterns until all of them are in constructor
  pattern form.  This function receives two lists of tasks, the second of which
  must be empty.  The result is a sequence a pair of answer lists which
  correspond to those arguments (the second of which is therefore empty).
  The second of each of these lists is used in recursion.
  @param cs The constraint set to use in checking compatibility.
  @param occurrences The set of occurrences observed so far in this computation.
  @param typ The type which is being checked for compatibility.
  @param tasks A list of tasks to be completed.
  @param ctr_tasks A list of tasks to be completed, all of which are constructor
                   patterns.
  @return A set containing triples between bindings, the results for the first
          task list, and the results for the second task list.
 *)
and compatibility_by_type_maybe_with_non_constr_pats
      (cs : Constraint_database.t)
      (occurrences : Occurrence_set.t)
      (typ : tbtype)
      (tasks : compatibility_task list)
      (ctr_tasks : compatibility_task list)
      : Internal_digest_result_set.t =
  Tiny_bang_logger.bracket_log logger `trace
    (sprintf
      "compatibility_by_type_maybe_with_non_constr_pats: Checking compatibility of %s\n    with tasks %s\n    and constr tasks %s"
      (pretty_type typ)
      (pretty_task_list tasks)
      (pretty_task_list ctr_tasks)
    )
    pretty_internal_result_set
  @@ fun () ->
  (* This function must recurse until it has eliminated all of the
     non-constructor patterns from the task list, at which point it sends the
     constructor patterns to compatibility_by_type_with_only_constr_pats.  Note
     that the tasks in ctr_tasks will be in reverse order by that point; that's
     fine, since the answers will be processed in reverse order as well. *)
  match tasks with
    | [] ->
        (* There are no tasks left to digest, so everything in the ctr_tasks
           list is a constructor pattern.  Use it to get our answers which are
           then processed on the way back down the recursion stack. *)
        let results = compatibility_by_type_with_only_constr_pats
                        cs occurrences typ ctr_tasks
        in
        results
          |> Compatibility_result_set.enum
          |> Enum.map
                (fun (Compatibility_result(bindings,answers)) ->
                  Internal_digest_result(bindings,[],answers))
          |> Internal_digest_result_set.of_enum
    | task::tasks' ->
        let Pattern_type(a,pfm) = task.ct_pat in
        (* TODO: handle the following potential Not_found more gracefully *)
        match Tvar_map.find a pfm with
          | Empty_filter_type ->
              (* EMPTY PATTERN RULE *)
              (* This rule always matches. *)
              if task.ct_expect_match = Must_not_match
                then
                  (* Then this case cannot yield useful proofs. *)
                  Internal_digest_result_set.empty
                else
                  (* Then we've trivially satisfied this pattern.  Remove it,
                     recurse, and then add a success to each result on the way
                     back. *)
                  let results = compatibility_by_type_maybe_with_non_constr_pats
                                  cs occurrences typ tasks' ctr_tasks
                  in
                  results
                    |> Internal_digest_result_set.map
                        (fun (Internal_digest_result(
                            bindings,answers,ctr_answers)) ->
                          Internal_digest_result(
                            bindings,true::answers,ctr_answers))
          | Conjunction_filter_type(a1,a2) ->
              (* CONJUNCTION PATTERN RULE *)
              (* To handle this correctly, we must split the conjunction task
                 into two smaller tasks.  The expectations of these smaller
                 tasks are dictated by the expectation of the larger task.  Once
                 these smaller tasks are processed, we must then join them to
                 maintain alignment for our caller. *)
              let left_expect,right_expect =
                (* Note that Must_not_match does not enforce anything about its
                   children; this is because *either* child may match as long as
                   the other does not. *)
                match task.ct_expect_match with
                  | Must_report -> (Must_report, Must_report)
                  | Must_match -> (Must_match, Must_match)
                  | Must_not_match -> (Must_report, Must_report)
              in
              let left_task = { ct_pat = Pattern_type(a1,pfm)
                              ; ct_binding = task.ct_binding
                              ; ct_expect_match = left_expect
                              } in
              let right_task = { ct_pat = Pattern_type(a2,pfm)
                               ; ct_binding = task.ct_binding
                               ; ct_expect_match = right_expect
                               } in
              (* Note that these smaller tasks may be non-constructor patterns,
                 so we put them on the left task list and allow recursion to
                 deal with them. *)
              let results = compatibility_by_type_maybe_with_non_constr_pats
                              cs occurrences typ 
                                (left_task::right_task::tasks') ctr_tasks
              in
              (* For each result that comes back, we need to join the first
                 two elements of the answers list. *)
              results
                |> Internal_digest_result_set.map
                      (fun (Internal_digest_result(
                          bindings,answers,ctr_answers)) ->
                        match answers with
                          | left_ans::right_ans::answers' ->
                              let new_answers =
                                (left_ans && right_ans)::answers' in
                              Internal_digest_result(
                                bindings,new_answers,ctr_answers)
                          | _ ->
                              raise (Invariant_failure
                                ("missing answer when processing conjunction " ^
                                "pattern results"))
                      )
          | _ ->
              (* In this case, the pattern is a constructor pattern.  We can
                 just shuffle it to the side and then do the same for the
                 answers on the way back. *)
              let results = compatibility_by_type_maybe_with_non_constr_pats
                              cs occurrences typ tasks' (task::ctr_tasks)
              in
              results
                |> Internal_digest_result_set.map
                      (fun (Internal_digest_result(
                          bindings,answers,ctr_answers)) ->
                        match ctr_answers with
                          | ctr_answer::ctr_answers' ->
                              Internal_digest_result(
                                bindings,ctr_answer::answers,ctr_answers')
                          | [] ->
                              raise (Invariant_failure
                                ("missing answer when processing constructor " ^
                                "pattern results!"))
                      )

(**
  Performs pattern matching in tandem on a type and a list of tasks which
  contain only constructor patterns.
  @param cs The constraint set to use in checking compatibility.
  @param occurrences The set of occurrences observed so far in this computation.
  @param typ The type which is being checked for compatibility.
  @param tasks A list of tasks to be completed, all of which must be constructor
               patterns.
  @return A set containing pairs between bindings and the results for the task
          list.
*)
and compatibility_by_type_with_only_constr_pats
      (cs : Constraint_database.t)
      (occurrences : Occurrence_set.t)
      (typ : tbtype)
      (tasks : compatibility_task list)
      : Compatibility_result_set.t =
  Tiny_bang_logger.bracket_log logger `trace
    (sprintf
      "compatibility_by_type_with_only_constr_pats: Checking compatibility of %s\n    with tasks %s"
      (pretty_type typ)
      (pretty_task_list tasks)
    )
    pretty_result_set
  @@ fun () ->
  (* At this point, the only tasks that exist have constructor patterns.  We
     begin by defining some utilities which are useful in this process. *)
  (** Solves each task immediately.  Given the list of tasks and a handler for
      a single pattern type, this function will produce a list of answers. *)
  let rec immediate_solve
        (f : pattern_type -> bool)
        (tasks : compatibility_task list)
        : Compatibility_result_set.t =
    let answers =
      enforce_list_size tasks @@
      tasks
        |> List.enum
        |> Enum.map
              (fun task -> f @@ task.ct_pat)
        |> List.of_enum
    in
    Compatibility_result_set.singleton
      (Compatibility_result([], answers))

  (** Solves each task for a single-argument type constructor.  This function
      takes a filter type handler which yields an inner variable on which to
      recurse.  It performs this recursion where appropriate.  When the
      recursive match is successful, the larger match is successful; if it is
      not or if the handler returns [None], the larger match fails. *)
  and recursive_single_constructor_solve
        (tv : tvar)
        (handler : pattern_filter_type -> tvar option)
        (tasks : compatibility_task list)
        (bind_state : bind_state)
        : Compatibility_result_set.t =
    logger `trace
      (sprintf
        "recursive_single_constructor_solve: Solving at %s and bind state %s\n    with tasks: %s"
        (pretty_tvar  tv)
        (pretty_bind_state bind_state)
        (pretty_task_list tasks)
      )
    ;
    let query_generator (task : compatibility_task)
          : bool compatibility_query option =
      let Pattern_type(a'',pfm) = task.ct_pat in
      let filt = Tvar_map.find a'' pfm in
      match handler filt with
        | Some a' ->
            let task' = { ct_pat = Pattern_type(a',pfm)
                        ; ct_binding = task.ct_binding
                        ; ct_expect_match = task.ct_expect_match
                        } in
            Some (Recursive_compatibility_query(task',
              fun answer ->
                if check_expectation task.ct_expect_match answer
                  then [answer] else []
            ))
        | None ->
            if check_expectation task.ct_expect_match false
              then Some (Immediate_answer false) else None
    in
    Tiny_bang_logger.bracket_log logger `trace
      (sprintf
        "recursive_single_constructor_solve: Invoking recursive_solve at %s and bind state %s\n    with tasks %s"
        (pretty_tvar  tv)
        (pretty_bind_state bind_state)
        (pretty_task_list tasks)
      )
      pretty_result_set
    @@ fun () ->
    recursive_solve tv query_generator tasks bind_state string_of_bool
      |> Enum.map (fun (bnd,ans) ->
            Compatibility_result(bnd,enforce_list_size tasks ans))
      |> Compatibility_result_set.of_enum

  (**
    Solves a recursive compatibility question.  Given a sequence of data (e.g.
    patterns) and a query generation function which yields recursive queries,
    this function executes a single recursive compatibility check and then
    aligns each recursive answer with the position of the input which generated
    the question.
    @param tv The type variable of the argument being checked for compatibility.
    @param query_generator The function which generates a recursive query for
                           each data element.  If [None] is yielded by this
                           query generator, the current computation is
                           considered defunct and [recursive_solve] yields no
                           results.
    @param data The sequence of data accepted by the query generator.
    @param bind_state The current binding state of this operation.
    @param pretty_output A pretty printer for the output type of the generated
                         queries.
    @return A compatibility result set.
  *)
  and recursive_solve
         : 'a 'b.
           tvar
        -> ('a -> 'b compatibility_query option)
        -> 'a list
        -> bind_state
        -> ('b -> string)
        -> (compatibility_bindings * 'b list) Enum.t =
    fun tv query_generator data bind_state pretty_output ->
      (* Begin by generating each of the possible queries for each of the
         corresponding inputs. *)
      let generated_queries = List.map query_generator data in
      if not @@ List.for_all (Option.is_some) generated_queries then
        (* In this case, one of the generators indicated that no sensible query
           could be made and that this computation is defunct. *)
        Enum.empty ()
      else
        let queries =
          generated_queries
            |> List.map
                  (fun qo -> match qo with
                              | Some q -> q
                              | None ->
                                  raise (Invariant_failure(
                                    "Found a None in the generated queries "^
                                    "list after they should've been noticed!"))
                  )
        in
        (* Next, extract the query list cases for each of those possibilities. *)
        Tiny_bang_logger.bracket_log logger `trace
          (sprintf
            "recursive_solve: Solving at %s with queries %s"
            (pretty_tvar tv)
            (pretty_list (pretty_query pretty_output) queries) 
          )
          (function result ->
              result
                |> Enum.clone
                |> List.of_enum
                |> pretty_list @@ pretty_tuple pretty_binding_set @@ pretty_list pretty_output
          )
        @@ function () ->
        (* At this point, we have a series of queries which give us
           recursive tasks to perform.  We *must* perform these tasks in
           tandem, so we'll extract the questions from the list and
           recurse. *)
        let tasks = List.filter_map
          (fun question ->
            match question with
              | Recursive_compatibility_query(task,_) -> Some task
              | Immediate_answer _ -> None)
          queries
        in
        let recursion_results =
          compatibility_by_tvar cs occurrences tv tasks bind_state
        in
        (* Now that we have the results from the recursive call, we need
           to use each answer list to produce an appropriate collection of
           answer lists for this routine.  Notably, the recursive
           compatibility queries consume an answer from the recursion
           result while the immediate answers do not.  Also, the recursive
           queries may yield multiple results; we must fold this
           non-determinism into our collection.  We start by generating
           for each answer a list of possible outcomes; we then take the
           Cartesian product of each outcome list.
        *)
        recursion_results
          |> Compatibility_result_set.enum
          |> Enum.map
              (fun (Compatibility_result(bindings,answers)) ->
                (* For each answer in the answer list above, we yield
                   the result of passing it through the appropriate
                   handler function.  For each answer, this produces a
                   list of possibilities. *)
                logger `trace
                  (sprintf
                    "recursive_solve: compatibility result:\n    bindings: %s\n    answers: %s"
                    (pretty_binding_set bindings)
                    (pretty_list string_of_bool answers)
                  );
                let possibilities_list =
                  (* We're going to accomplish this by keeping a stack
                     of available result handlers.  If the original
                     query was immediate, we need not consume a
                     handler; if it was a recursive result, it does. *)
                  List.rev @@ fst @@ List.fold_left
                    (fun (results, rec_answers') -> fun query ->
                      match query with
                        | Recursive_compatibility_query(_,f) ->
                            begin
                              match rec_answers' with
                                | [] -> raise (Invariant_failure
                                          "missing answers in recursive_solve")
                                | rec_ans::rec_answers'' ->
                                    ((f rec_ans :: results),rec_answers'')
                            end
                        | Immediate_answer ans ->
                            ([ans] :: results,rec_answers')
                    )
                    ([],answers) queries
                in
                (* We now have a list where each position represents all
                   of the possibilities at that index.  For instance, if
                   the list is of the form [[1;2];[3];[4;5]] then we
                   know that the first position of the list may be
                   either a 1 or a 2, the second is always a 3, and so
                   on.  We need to flatten out all of these
                   possibilities.  Fortunately, we have a utility 
                   function for that. *)
                logger `trace
                  (sprintf
                    "recursive_solve: original possibilities: %s" @@
                    pretty_list
                      (pretty_list pretty_output)
                      possibilities_list
                  );
                let results_lists =
                  cartesian_product_of_list possibilities_list in
                logger `trace
                  (sprintf
                    "recursive_solve: Cartesian product of possibilities: %s" @@
                    pretty_list
                      (pretty_list pretty_output)
                      possibilities_list
                  );
                (* Now enumerate this structure so we can merge it with
                   the ones from the other compatibility results.  Pair
                   up the bindings with each of these lists so we don't
                   lose them. *)
                results_lists
                  |> List.enum
                  |> Enum.map (fun lst -> (bindings,lst))
              )
          |> Enum.concat
  in
  match typ with
    | Empty_onion_type ->
        immediate_solve (fun x -> false) tasks
    | Label_type(l,a) ->
        recursive_single_constructor_solve a
          (fun filt -> match filt with
                        | Label_filter_type(l',a') ->
                            if l = l' then Some a' else None
                        | _ -> None)
          tasks
          Do_bind
    | Function_type(_,_,_) ->
        immediate_solve (fun x -> false) tasks
    | Onion_type(a1,a2) ->
        (* This is the hard case and the whole reason that compatibility is
           structured in this module the way that it is.  The formal definition
           of this relation requires a non-deterministic choice to be made at
           this point: we must partition the positively-matching set such that
           those patterns we attribute to the right side do not match the left
           side.  Testing all of the possible partitionings is exponential in
           complexity and so clearly infeasible.
          
           Our strategy here is to use the task structure around which this
           module is designed to discover the correct partitionings.  Every
           task which Must_not_match is sent in that form to both sides of the
           onion.  The other tasks (Must_match or Must_report) are sent as
           Must_report to a recursive query on the left onion variable.  Then,
           each of those which *failed* is sent to the right with its original
           expectation.  Finally, we aggregate the results.
        *)
        let left_query_generator task =
          (* This is the first task our query will perform. *)
          let left_task = { ct_pat = task.ct_pat
                          ; ct_binding = task.ct_binding
                          ; ct_expect_match =
                              if task.ct_expect_match = Must_not_match
                                then Must_not_match
                                else Must_report
                          } in
          (* Once the query has executed that task, this function will use
             the results to generate queries for the right side. *)
          let query_result_handler ans =
            match task.ct_expect_match with
              | Must_not_match ->
                  if ans
                    then
                      (* This shouldn't have happened; we need this not to
                         match.  This part of the proof search is fruitless. *)
                      []
                    else
                      (* Then we've satisfied this requirement.  We now need to
                         generate a task which proves that the pattern does not
                         match the right side.  Conveniently, left_task is
                         also applicable here. *)
                      [Recursive_compatibility_query(left_task,
                        fun answer -> if answer then [] else [false]
                        )]
              | _ ->
                  (* In this case, we were testing the right side to see if it
                     stuck.  If the pattern matched, we can exclude it from the
                     right side's testing; if it didn't, then we send it into
                     the right side with its original expectation. *)
                  if ans
                    then [Immediate_answer true]
                    else
                      (* We need to give a task which tests the pattern on the
                         right side using the same expectation as the original.
                         As it turns out, the original task is suited for
                         this. *)
                      [Recursive_compatibility_query(task,
                        fun answer ->
                          if answer || task.ct_expect_match = Must_report
                            then [answer]
                            else []
                        )]
          in
          (* Now we build the query that we need to generate. *)
          Some(Recursive_compatibility_query(left_task, query_result_handler))
        in
        let results_left =
          recursive_solve
            a1
            left_query_generator
            tasks
            Dont_bind
            (pretty_query string_of_bool)
        in
        results_left
          |> Enum.map
              (fun (bindings_left, right_side_queries) ->
                let results_right =
                  recursive_solve
                    a2
                    Option.some
                    right_side_queries
                    Dont_bind
                    string_of_bool
                in
                results_right
                  |> Enum.map
                      (fun (bindings_right, answers) ->
                        let bindings = bindings_left @ bindings_right in
                        Compatibility_result(bindings, answers)
                      )
              )
          |> Enum.concat
          |> Compatibility_result_set.of_enum
;;


(* ************************************************************************** *)
(* INTERFACE *)
(* This section contains the publicly-exposed functions for this module. *)

(**
  This function models the behavior of compatibility in the type system.
  Results are returned in the form of a set of constraint set options: sets
  represent bindings whereas [None] represents a constructive case of failure.
  @param a The type variable for the argument.
  @param cs The constraint set in context for compatibility.
  @param pat_b The binding pattern for this check.
  @param pats_n The negative patterns for this check.
  @return A list of possible bindings; each element represents a compatibility
          proof's results.
*)
let find_compatibility_cases
      (a : tvar)
      (cs : Constraint_database.t)
      (pat_b : pattern_type)
      (pats_n : Pattern_type_set.t)
      : compatibility_bindings option list =
  let task_b = compatibility_task pat_b true Must_report in
  let tasks_n = List.map
                  (fun p -> compatibility_task p false Must_not_match)
                  (Pattern_type_set.elements pats_n) in
  let results = compatibility_by_tvar
                  cs Occurrence_set.empty a (task_b :: tasks_n) Do_bind in
  results
    |> Compatibility_result_set.enum
    |> Enum.map (fun (Compatibility_result(bindings,answers)) ->
                    if List.first answers then Some bindings else None)
    |> List.of_enum
;;

(**
  A computable function to determine whether a given restricted type is
  "sensible" according to the theory (Definition 3.28 of Building a Typed
  Scripting Language).
  @param ft The filtered type to test.
  @param cs The constraint set in the context of which the filtered type is to
            be evaluated.
  @return [true] if the type is sensible; [false] if it is not.
*)
let sensible
      (ft : filtered_type)
      (cs : Constraint_database.t)
      : bool =
  let Filtered_type(t,filts_pos,filts_neg) = ft in
  let tasks_pos = List.map
                    (fun p -> compatibility_task p false Must_match)
                    (Pattern_type_set.elements filts_pos) in
  let tasks_neg = List.map
                    (fun p -> compatibility_task p false Must_not_match)
                    (Pattern_type_set.elements filts_neg) in
  let results = compatibility_by_type
                  cs Occurrence_set.empty t (tasks_pos @ tasks_neg) Do_bind in
  not @@ Compatibility_result_set.is_empty results
;;
