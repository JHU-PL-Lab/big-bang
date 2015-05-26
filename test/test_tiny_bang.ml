open OUnit2

let all_tests =
  [ Test_little_bang_a_translator.tests
  ; Test_tiny_bang_utils.tests
  ; Test_tiny_bang_files.tests
  ];;

run_test_tt_main ("Tiny_bang" >::: all_tests);;
