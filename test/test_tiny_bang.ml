open OUnit

let all_tests =
  [
    Test_tiny_bang_utils.tests
  ]

let () =
  ignore(run_test_tt ("Tiny_bang" >::: all_tests))
