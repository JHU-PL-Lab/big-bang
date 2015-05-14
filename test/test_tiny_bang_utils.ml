open OUnit

let natural_compare_seq_returns_0_for_empty_list () =
  assert_equal 0 (Tiny_bang_utils.natural_compare_seq [])

let tests = "Tiny_bang_utils" >::: [
      "natural_compare_seq returns 0 for empty list" >:: natural_compare_seq_returns_0_for_empty_list;
    ]
