open OUnit2
open Batteries

open Tiny_bang_ast;;
open Tiny_bang_compatibility;;
open Tiny_bang_types;;
open Tiny_bang_string_utils;;
open Tiny_bang_utils;;

let make_var_util str = Tvar(Ident(str),None);;

(*******************************************Pattern definitions*****************************************)
(* () pattern *)
let empty_onion_pattern = 
	let p0 = make_var_util "p0" in 
	Pattern_type(p0,(Tvar_map.of_enum @@ List.enum [(p0,Empty_filter_type)]))
;;

(* 'A() pattern *)
let a_unit_pattern = 
	let p2 = make_var_util "p2" in
	let p1 = make_var_util "p1" in
	Pattern_type(p1,(Tvar_map.of_enum @@ List.enum 
						[
							(p1,Label_filter_type(Label(Ident("A")),p2));
							(p2,Empty_filter_type);
						]
					)
				)
;;

(* 'B() pattern *)
let b_unit_pattern = 
	let p7 = make_var_util "p2" in
	let p8 = make_var_util "p1" in
	Pattern_type(p8,(Tvar_map.of_enum @@ List.enum 
						[
							(p8,Label_filter_type(Label(Ident("B")),p7));
							(p7,Empty_filter_type);
						]
					)
				)
;;


(* 'A()*'B() pattern *)
let a_unit_conjunction_b_unit_pattern =
	let p3 = make_var_util "p3" in
	let p4 = make_var_util "p4" in
	let p5 = make_var_util "p5" in
	let p6 = make_var_util "p6" in
	Pattern_type(p3,(Tvar_map.of_enum @@ List.enum
						[
							(p3,Conjunction_filter_type (p4,p5));
							(p4,Label_filter_type(Label(Ident("A")),p6));
							(p6,Empty_filter_type);
							(p5,Label_filter_type(Label(Ident("B")),p6));
						]
					)
				)
;;

(* '`A (α * `B α') with bindings `B () <: α and () <: α' pattern *)
let custom_pattern = 
	let p7 = make_var_util "p7" in
	let p8 = make_var_util "p8" in
	let p9 = make_var_util "p9" in
	let p10 = make_var_util "p10" in
	let p11 = make_var_util "p11" in
	Pattern_type(p7,(Tvar_map.of_enum @@ List.enum
						[
							(p7,Label_filter_type(Label(Ident("A")),p8));
							(p8,Conjunction_filter_type(p9,p10));
							(p9,Label_filter_type(Label(Ident("B")),p11));
							(p11,Empty_filter_type);
							(p10,Label_filter_type(Label(Ident("B")),p11))
						]
					)
				)
;;

(****************************************Type definitions*******************************************************)
(* 'A() type *)
let a_unit_type =
	let a0 = make_var_util "a0" in
	let a1 = make_var_util "a1" in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a0),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
								(* Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)])),
								Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)])) *)
							)),a1);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a0);]
	in 
	(* raise(Not_yet_implemented "a_unit_type") *)
	(a1, Constraint_database.of_enum @@ List.enum c_list)
;;

(* 'B() type *)
let b_unit_type =
	let a2 = make_var_util "a2" in
	let a3 = make_var_util "a3" in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("B")),a2),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
								(* Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)])),
								Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)])) *)
							)),a3);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a2);]
	in 
	(* raise(Not_yet_implemented "a_unit_type") *)
	(a3, Constraint_database.of_enum @@ List.enum c_list)
;;

(* 'A()U'B() type *)
let a_unit_union_b_unit_type = 
	let a4 = make_var_util "a4" in
	let a5 = make_var_util "a5" in
	let a6 = make_var_util "a6" in
	let a7 = make_var_util "a7" in
	let a8 = make_var_util "a8" in
	let c_list = [ (* Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a4),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a5);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("B")),a4),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a5); 
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a4); *)

				Lower_bound_constraint
					(Intermediate_lower_bound (a4),a5);
				Lower_bound_constraint
					(Intermediate_lower_bound (a6),a5);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a7),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a4);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("B")),a8),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a6);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a7);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a8);
				
				]
	in
	(a5, Constraint_database.of_enum @@ List.enum c_list)
;;	

(* 'A()&'B() type *)
let a_unit_onion_b_unit_type =
	let a9 = make_var_util "a9" in
	let a10 = make_var_util "a10" in
	let a11 = make_var_util "a11" in
	let a12 = make_var_util "a12" in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Onion_type (a9,a10),
							Pattern_type_set.empty,
							Pattern_type_set.empty
						)),a11);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a12),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a9);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Label_type
								(Label(Ident("B")),a12),
								Pattern_type_set.empty,
								Pattern_type_set.empty
							)),a10);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a12);
				]
	in
	(a11, Constraint_database.of_enum @@ List.enum c_list)
;;
(* 'A'B() type*)
let a_b_unit_type =
	let a13 = make_var_util "a13" in
	let a14 = make_var_util "a14" in
	let a15 = make_var_util "a15" in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a13),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
							)),a14);
				Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("B")),a15),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
							)),a13);	
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a15);
				]
	in 
	(* raise(Not_yet_implemented "a_unit_type") *)
	(a14, Constraint_database.of_enum @@ List.enum c_list)
;;


(* any function type here it is ()->'A() *)
let func_type =
	let a16 = make_var_util "a16" in
	let a17 = make_var_util "a17" in
	let a18 = make_var_util "a18" in
	(* let a19 = make_var_util "a19" in *)
	let c_list_for_func = [Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("A")),a18),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
							)),a16);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a18);]
	in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Function_type 
								(empty_onion_pattern, 
								 a16,
								 Constraint_database.of_enum @@ List.enum c_list_for_func),
							Pattern_type_set.empty,
							Pattern_type_set.empty
							)),a17);
				]
	in
	(a17, Constraint_database.of_enum @@ List.enum c_list)
;;

(************************************Test functions for matching patterns and types***************************************)

(* testing the match between the pattern () with type 'A() *)
let test_a_matches_empty _ = 
	let results = uncurry find_compatibility_cases
	 				a_unit_type 
	 				empty_onion_pattern 
	 				Pattern_type_set.empty
	in
	match results with
	| [Some _] -> ()
	| _ -> assert_failure "A() ~ () did not produce a single success"
;; 
(* testing the match between the pattern 'A() with the type 'A() *)
let test_a_matches_a _ =
	let results = uncurry find_compatibility_cases
					a_unit_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	match results with
	| [Some _] -> ()
	| _ -> assert_failure "A() ~ 'A() did not produce a single success"
;; 

let test_a_not_antimatches_a _ =
	let results = uncurry find_compatibility_cases
					a_unit_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	if (List.exists Option.is_none results)
		then assert_failure "A() !!~ 'A() did not produce a single success"
		else ()
;;

let test_b_not_matches_a _ =
	let results = uncurry find_compatibility_cases
					b_unit_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	match results with
	| [None] -> ()
	| _ -> assert_failure "B() ~ 'A() matches"
	
;;

let test_b_antimatches_a _ =
	let results = uncurry find_compatibility_cases
					b_unit_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	if (List.exists Option.is_none results)
		then ()
		else assert_failure "B() ~ does not antimatch 'A()"
;;

let test_a_union_b_antimatches_a_conjunction_b _ =
	let results = uncurry find_compatibility_cases
					a_unit_union_b_unit_type
					a_unit_conjunction_b_unit_pattern
					Pattern_type_set.empty
	in
	if (List.exists Option.is_none results)
		then ()
		else assert_failure "A()U'B() matches 'A()*'B()"
;;

let test_a_union_b_matches_a _ =
	let results = uncurry find_compatibility_cases
					a_unit_union_b_unit_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	(*let predicate_none = fun c_list_ele ->(Option.is_none c_list_ele) in *)
	if (List.exists Option.is_some results) (* && (List.exists predicate_none results) *)
		then ()
		else assert_failure "A()U'B() does not matches 'A()"
;;

let test_a_union_b_not_antimatches_a_b _ =
	let pattern_set_with_a_unit_pattern = Pattern_type_set.add a_unit_pattern Pattern_type_set.empty in
	let results = uncurry find_compatibility_cases
					a_unit_union_b_unit_type
					empty_onion_pattern
					(Pattern_type_set.add b_unit_pattern pattern_set_with_a_unit_pattern)
	in
	if (List.exists Option.is_none results)
		then assert_failure "A()U'B() does not antimatch {'A(),'B()}"
		else ()
;;

let test_a_onion_b_matches_a_conjunction_b _ =
	let results = uncurry find_compatibility_cases
					a_unit_onion_b_unit_type
					a_unit_conjunction_b_unit_pattern
					Pattern_type_set.empty
	in
	match results with
	| [Some _] -> ()
	| _ -> assert_failure "A()&'B() does not matches with 'A()*'B()"
;;

let test_a_onion_b_without_a_conjunction_b_does_not_relate_to_empty _ =
	let results = uncurry find_compatibility_cases
					a_unit_onion_b_unit_type
					empty_onion_pattern
					(Pattern_type_set.add a_unit_conjunction_b_unit_pattern Pattern_type_set.empty)
	in
	match results with
	| [] -> ()
	| _ -> assert_failure "A()&'B() - 'A()*'B() relates to empty pattern"
;;

let test_a_onion_b_not_antimatches_a_conjunction_b _ =
	let results = uncurry find_compatibility_cases
					a_unit_onion_b_unit_type
					a_unit_conjunction_b_unit_pattern
					Pattern_type_set.empty
	in
	if (List.exists Option.is_none results)
		then assert_failure "A()&'B() does not antimatch 'A()*'B()"
		else ()
;;

let test_func_type_antimatches_a _ =
	let results = uncurry find_compatibility_cases
					func_type
					a_unit_pattern
					Pattern_type_set.empty
	in
	if List.exists Option.is_none results
		then ()
		else assert_failure "func_type matches 'A() pattern"
;;

let test_a_b_unit_matches_custom_pattern _ =
	let results = uncurry find_compatibility_cases
					a_b_unit_type
					custom_pattern
					Pattern_type_set.empty
	in
	match results with
	| [Some _] -> ()
	| _ -> assert_failure "A'B() does not match custom patterm"
;;

let tests = "Tiny_bang_compatibility" >::: [
	"A() ~ () " >:: test_a_matches_empty;
	"A() ~ 'A()" >:: test_a_matches_a;
	"A() !!~ 'A()" >:: test_a_not_antimatches_a;
	"B() ~ 'A()" >:: test_b_not_matches_a;
	"B() !~ 'A()" >:: test_b_antimatches_a;
	"A()U'B() !~ 'A()*'B()" >:: test_a_union_b_antimatches_a_conjunction_b;
	"A()U'B() ~ 'A()" >:: test_a_union_b_matches_a;
	"A()U'B() !!~ {'A(),'B()}" >:: test_a_union_b_not_antimatches_a_b;
	"A()&'B() ~ 'A()*'B()" >:: test_a_onion_b_matches_a_conjunction_b;
	"A()&'B() ~!! 'A()*'B()" >:: test_a_onion_b_not_antimatches_a_conjunction_b;
	"func_type !~ 'A() " >:: test_func_type_antimatches_a;
	"A'B() ~ custom pattern" >:: test_a_b_unit_matches_custom_pattern;
]
;;
