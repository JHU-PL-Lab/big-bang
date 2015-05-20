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

open Tiny_bang_ast_wellformedness;;
open Tiny_bang_interpreter;;
open Tiny_bang_parser;;
open Tiny_bang_typechecker;;
open Tiny_bang_utils;;

exception File_test_creation_failure of string;;

type test_expectation =
  | Expect_typecheck_only
  | Expect_typecheck_and_run
  | Expect_typefail;;

let parse_expectation str =
  match str with
    | "EXPECT-TYPECHECK" -> Some(Expect_typecheck_only)
    | "EXPECT-TYPECHECK-ONLY" -> Some(Expect_typecheck_and_run)
    | "EXPECT-TYPEFAIL" -> Some(Expect_typefail)
    | _ -> None
;;

let make_test filename expectation =
  let test_name_expectation = match expectation with
                                | Expect_typecheck_only ->
                                    "(should typecheck)"
                                | Expect_typecheck_and_run ->
                                    "(should typecheck and run)"
                                | Expect_typefail ->
                                    "(should fail to typecheck)"
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
      check_wellformed_expr expr;
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
        | Expect_typefail ->
            assert_bool "Typechecking succeeded." @@ not typecheck_result

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
