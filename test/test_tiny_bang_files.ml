(**
  This test module will load a series of test files from the test sources
  directory and execute them one by one.
  
  Each file is expected to contain a comment describing the expected test
  result.  The comment should be of one of the following forms:
  
    - [EXPECT-TYPECHECK-ONLY] (which requires that the code typechecks)
    - [EXPECT-TYPECHECK] (which requires that the code typechecks {i and} runs)
    - [EXPECT-TYPEFAIL] (which requires that the code fails to typecheck)
*)

open Batteries;;
open OUnit2;;

open Tiny_bang_ast;;
open Tiny_bang_ast_wellformedness;;
open Tiny_bang_interpreter;;
open Tiny_bang_typechecker;;

exception File_test_creation_failure of string;;
exception Parse_expectation_failure of string;;

type test_expectation =
  | Expect_typecheck_only
  | Expect_typecheck_and_run
  | Expect_typefail
  | Expect_int_variable_values of (string * int) list
  | Expect_illformed
;;

let parse_expectation str =
  (* e.g. EXPECT-INT-VARIABLE-VALUE c=9 d=10 devil=666 *)
  if String.starts_with str "EXPECT-INT-VARIABLE-VALUE" then
    Some (Expect_int_variable_values (List.map
      (fun term -> match Str.split (Str.regexp_string "=") term with
        | [x;y] -> (x, int_of_string y)
        | _ -> raise @@ Parse_expectation_failure "Invalid EXPECT-INT-VARIABLE-VALUE format"
      )
      (Str.split (Str.regexp_string " ") (String.tail str (String.length "EXPECT-INT-VARIABLE-Value")))
    ))
  else
    match str with
      | "EXPECT-TYPECHECK" -> Some(Expect_typecheck_and_run)
      | "EXPECT-TYPECHECK-ONLY" -> Some(Expect_typecheck_only)
      | "EXPECT-TYPEFAIL" -> Some(Expect_typefail)
      | "EXPECT-ILL-FORMED" -> Some(Expect_illformed)
      | _ -> None
;;

let make_test filename expectation =
  let test_name_expectation =
    match expectation with
    | Expect_typecheck_only ->
        "(should typecheck)"
    | Expect_typecheck_and_run ->
        "(should typecheck and run)"
    | Expect_typefail ->
        "(should fail to typecheck)"
    | Expect_int_variable_values(lst) ->
        "(should have " ^ (String.concat " AND " (List.map
          (function (x,y) -> "`" ^ x ^ "'=" ^ string_of_int y) lst)) ^ ")"
    | Expect_illformed ->
        "(should have ill-formedness)"
  in
  let test_name = filename ^ ": " ^ test_name_expectation in
  (* Create the test in a thunk. *)
  test_name >::
    function _ ->
      (* Begin by parsing the file. *)
      let (expr,_) =
        File.with_file_in filename Tiny_bang_parser.parse_tiny_bang_program
      in
      (* Verify that it is well-formed. *)
      begin
        try
          check_wellformed_expr expr;
          begin
            match expectation with
                | Expect_illformed -> assert_failure @@ "No ill-formedness found."
                | _ -> ()
          end
        with Illformedness_found(illformednesses) ->
          begin
            match expectation with
                | Expect_illformed -> ()
                | _ -> assert_failure ("Ill-formedness program:" ^
                                       (illformednesses
                                        |> List.map pretty_illformedness
                                        |> String.join ", "))
          end
      end;
      (* Next, typecheck it. *)
      let typecheck_result = typecheck expr in
      match expectation with
        | Expect_typecheck_only ->
            assert_bool "Typechecking failed." typecheck_result
        | Expect_typecheck_and_run ->
            assert_bool "Typechecking failed." typecheck_result;
            (* Now actually run it!  This won't produce any assertable results,
               but it could raise an exception if the typechecker is broken. *)
            ignore (eval expr)
        | Expect_int_variable_values(lst) ->
            List.iter (function (name, value) ->
              (match eval expr with
                | (_, env) ->
                  (* I don't use lookup here, since all we have is the name of
                     variable we want to look at, not any other info in the Var
                     structure. *)
                  let variable = find (function
                    | Var (_, Ident(identifier), _) -> identifier = name
                    | _ -> false
                  ) (Environment.keys env)
                  in
                  (match var_project project_int env variable with
                    | None -> assert_failure "There isn't an int at the variable"
                    | Some(actual) -> assert_equal actual value))
            ) lst
        | Expect_typefail ->
          assert_bool "Typechecking succeeded." @@ not typecheck_result
        | Expect_illformed ->
          ()

let make_test_from filename =
  let expectations =
    filename
      |> File.lines_of
      |> Enum.filter_map
          (fun str ->
            let str' = String.trim str in
            if String.starts_with str' "#"
              then
                let str'' = String.trim @@ String.tail str' 1 in
                parse_expectation str''
              else None
          )
      |> List.of_enum
  in
  match expectations with
    | [expectation] ->
        make_test filename expectation
    | [] ->
        raise (File_test_creation_failure(
          "Could not create test from file " ^ filename ^
          ": no expectation comment found."))
    | _ ->
        raise (File_test_creation_failure(
          "Could not create test from file " ^ filename ^
          ": multiple expectation comments found."))
          
let make_all_tests pathname =
  if Sys.file_exists pathname && Sys.is_directory pathname
    then
      Sys.files_of pathname
        |> Enum.map (fun f -> pathname ^ Filename.dir_sep ^ f)
        |> Enum.filter (fun f -> not @@ Sys.is_directory f)
        |> Enum.filter (fun f -> String.ends_with f ".tb")
        |> Enum.map make_test_from
        |> List.of_enum
    else
      raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_tiny_bang_files" >::: make_all_tests "test-sources";;
