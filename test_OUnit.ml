(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright 2002, 2003 Maas-Maarten Zeeman. All rights reserved. See  *) 
(* LICENCE for details.                                                *)
(***********************************************************************)

(* $Id: test_OUnit.ml,v 1.11 2004/06/29 08:26:20 maas Exp $ *)

open OUnit
  
let test_case = TestCase (fun () -> ())
let labeled_test_case = "label" >: test_case
let suite_a = "suite_a" >: TestList [test_case]
let suite_b = "suite_b" >: TestList [labeled_test_case]
let suite_c = "suite_c" >: TestList [test_case; labeled_test_case]
let suite_d = "suite_d" >: TestList [suite_a; suite_c]  

let rec string_of_paths = function
    [] -> ""
  | h::t -> (string_of_path h) ^ "\n" ^ (string_of_paths t)

(* Test which checks if the test case count function works correctly *)
let test_case_count _ =
  let assert_equal ?msg = assert_equal ?msg ~printer:string_of_int in
  assert_equal 0 (test_case_count (TestList []));
  assert_equal 0 (test_case_count (TestLabel("label", TestList [])));
  assert_equal 0 (test_case_count (TestList [TestList []; 
					     TestList [TestList []]]));

  assert_equal 1 (test_case_count test_case);
  assert_equal 1 (test_case_count labeled_test_case);
  assert_equal 1 (test_case_count suite_a);
  assert_equal 1 (test_case_count suite_b);
  
  assert_equal 1 (test_case_count (TestList [suite_a; TestList []]));
  assert_equal 1 (test_case_count (TestList [TestList []; 
					     TestList [suite_b]]));
  assert_equal 2 (test_case_count suite_c);
  assert_equal 3 (test_case_count suite_d)

(* Test which checks if the paths are correctly constructed *)
let test_case_paths _ =
      (* A single testcase results in a list countaining an empty list *)
  let assert_equal ?msg = assert_equal ?msg ~printer:string_of_paths in
  assert_equal [[]] (test_case_paths test_case);
  assert_equal [[Label "label"]] 
    (test_case_paths labeled_test_case);
  assert_equal [[ListItem 0; Label "suite_a"]] 
    (test_case_paths suite_a);
  assert_equal [[Label "label"; ListItem 0; Label "suite_b"]] 
    (test_case_paths suite_b);
  assert_equal [[ListItem 0; Label "suite_c"]; 
		[Label "label"; ListItem 1; Label "suite_c"]] 
    (test_case_paths suite_c);
  assert_equal [[ListItem 0; Label "suite_a"; ListItem 0; Label "suite_d"];
		[ListItem 0; Label "suite_c"; ListItem 1; Label "suite_d"];
		[Label "label"; ListItem 1; Label "suite_c"; ListItem 1;
		 Label "suite_d"]] 
    (test_case_paths suite_d)

let test_assert_raises _ =
  assert_raises 
    (Failure "OUnit: expected: Failure(\"Boo\") but got: Failure(\"Foo\")") 
    (fun _ -> (assert_raises (Failure "Boo") (fun _ -> raise (Failure "Foo"))));
  assert_raises 
    (Failure "OUnit: A label\nexpected: Failure(\"Boo\") but got: Failure(\"Foo\")") 
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") (fun _ -> raise (Failure "Foo"))));
  assert_raises 
    (Failure "OUnit: expected exception Failure(\"Boo\"), but no exception was not raised.") 
    (fun _ -> (assert_raises (Failure "Boo") (fun _ -> ())));
  assert_raises 
    (Failure "OUnit: A label\nexpected exception Failure(\"Boo\"), but no exception was not raised.") 
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") (fun _ -> ())))

let test_cmp_float _ =
  assert_equal ~cmp: cmp_float 0.0001 0.0001;
  assert_equal ~cmp: (cmp_float ~epsilon: 0.001) 1.0001 1.00001;
  assert_raises (Failure "OUnit: not equal") 
      (fun _ -> assert_equal ~cmp: cmp_float 100.0001 101.001)

(* Construct the test suite *)
let suite = "OUnit" >::: [ "test_case_count" >:: test_case_count;
			   "test_case_paths" >:: test_case_paths;
			   "test_assert_raises" >:: test_assert_raises;
			   "test_cmp_float" >:: test_cmp_float;
			 ]

(* Run the tests in test suite *)
let _ = run_test_tt_main suite
