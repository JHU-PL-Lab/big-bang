open OUnit2

let natural_compare_seq_returns_0_for_empty_list _ =
  assert_equal 0 (Tiny_bang_utils.natural_compare_seq [])
  
let pretty_list_list pretty lst =
    "[" ^
    (List.fold_left
        (fun a l -> a ^ (if String.length a > 0 then ";" else "") ^ "[" ^
            (List.fold_left
                (fun a x -> a ^ (if String.length a > 0 then ";" else "") ^ pretty x)
                "" l)
            ^ "]"
        )
        "" lst)
    ^ "]"
;;

let cartesian_product_tests =
  let do_test lst1 lst2 _ =
    assert_equal (Tiny_bang_utils.cartesian_product_of_list lst1) lst2
  in
  let make_test (lst1,lst2) =
    let test_name = "cartesian_product_of_list " ^
                    pretty_list_list string_of_int lst1 ^ " = " ^
                    pretty_list_list string_of_int lst2
    in
    test_name >:: do_test lst1 lst2
  in
  List.map make_test
    [ ( []
      , [[]]
      )
    ; ( [[1]]
      , [[1]]
      )
    ; ( [[1;2]]
      , [[1];[2]]
      )
    ; ( [[1;2];[3]]
      , [[1;3];[2;3]]
      )
    ; ( [[1;2];[3];[4;5;6]]
      , [[1;3;4];[2;3;4];[1;3;5];[2;3;5];[1;3;6];[2;3;6]]
      )
    ; ( [[1;2];[];[4;5;6]]
      , []
      )
    ]

let tests = "Tiny_bang_utils" >::: [
      "natural_compare_seq returns 0 for empty list" >:: natural_compare_seq_returns_0_for_empty_list;
    ] @
    cartesian_product_tests
;;
