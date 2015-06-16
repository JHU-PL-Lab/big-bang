open OUnit2
open Batteries

open Tiny_bang_ast;;
open Tiny_bang_compatibility;;
open Tiny_bang_types;;
open Tiny_bang_utils;;

let make_var_util str = Tvar(Ident(str),None);;

(* () pattern *)
let empty_onion_pattern = 
	let a0 = make_var_util "a0" in 
	Pattern_type(a0,(Tvar_map.of_enum @@ List.enum [(a0,Empty_filter_type)]))
;;
(* 'A() type *)
let a_unit_type =
	let a1 = make_var_util "a1" in
	let a2 = make_var_util "a2" in
	let c_list = [Lower_bound_constraint
					(Type_lower_bound 
						(Filtered_type
							(Label_type
								(Label(Ident("'A")),a1),
								Pattern_type_set.empty ,
								Pattern_type_set.empty
								(*Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)])),
								Pattern_type(a2,(Tvar_map.of_enum @@ List.enum[(a2,Empty_filter_type)]))*)
							)),a2);
				Lower_bound_constraint
					(Type_lower_bound
						(Filtered_type
							(Empty_onion_type,
							Pattern_type_set.empty ,
							Pattern_type_set.empty
							)),a1)]
	in 
	(* raise(Not_yet_implemented "a_unit_type") *)
	(a2, Constraint_database.of_enum @@ List.enum c_list)
;;

let test_a_matches_empty _ = 
	let results = uncurry find_compatibility_cases
	 				a_unit_type 
	 				empty_onion_pattern 
	 				Pattern_type_set.empty
	in
	match results with
	| [Some _] -> ()
	| _ -> assert_failure "'A() ~ () did not produce a single success"
;; 


let tests = "Tiny_bang_compatibility" >::: [
	"'A() ~ () " >:: test_a_matches_empty
]
;;
