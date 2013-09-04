
open OUnitTest
open OUnitAssert
open OUnit2

let test_assert_raises _ =
  assert_raises
    (Failure "OUnit: expected: Failure(\"Boo\") but got: Failure(\"Foo\")")
    (fun _ -> (assert_raises (Failure "Boo")
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises
    (Failure
       "OUnit: A label\nexpected: Failure(\"Boo\") but got: Failure(\"Foo\")")
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo")
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises
    (Failure
       "OUnit: expected exception Failure(\"Boo\"), \
        but no exception was raised.")
    (fun _ -> (assert_raises (Failure "Boo") (fun _ -> ())));
  assert_raises
    (Failure "OUnit: A label\nexpected exception Failure(\"Boo\"), \
              but no exception was raised.")
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") (fun _ -> ())))

(* Test the float compare, and use the cmp label *)
let test_cmp_float _ =
  assert_equal ~cmp: cmp_float 0.0001 0.0001;
  assert_equal ~cmp: (cmp_float ~epsilon: 0.001) 1.0001 1.00001;
  assert_raises (Failure "OUnit: not equal")
      (fun _ -> assert_equal ~cmp: cmp_float 100.0001 101.001)

let test_assert_string _ =
  assert_string "";
  assert_raises (Failure "OUnit: A string")
    (fun _ -> assert_string "A string")

let test_assert_bool _ =
  assert_bool "true" true;
  assert_raises (Failure "OUnit: false") (fun _ -> assert_bool "false" false)

let test_case_skip ctxt =
  begin
    try
      skip_if false "test"
    with _ ->
      assert_failure "Should not skip this test."
  end;
  assert_raises (Skip "test") (fun _ -> skip_if true "test")

let test_case_todo ctxt =
  assert_raises (Todo "test") (fun _ -> todo "test")

let test_assert_command ctxt =
  assert_command ~ctxt Sys.executable_name ["-help"]

let tests =
  "OUnitAssert" >:::
  [ "test_assert_raises" >:: test_assert_raises;
    "test_assert_string" >:: test_assert_string;
    "test_assert_bool" >:: test_assert_bool;
    "test_cmp_float" >:: test_cmp_float;
    "test_case_skip" >:: test_case_skip;
    "test_case_todo" >:: test_case_todo;
    "test_assert_command" >:: test_assert_command;
  ]
