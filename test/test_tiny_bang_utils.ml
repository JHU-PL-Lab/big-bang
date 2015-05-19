open OUnit2

let natural_compare_seq_returns_0_for_empty_list _ =
  assert_equal 0 (Tiny_bang_utils.natural_compare_seq [])

let tests = "Tiny_bang_utils" >::: [
      "natural_compare_seq returns 0 for empty list" >:: natural_compare_seq_returns_0_for_empty_list;
    ]
