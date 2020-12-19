(**************************************************************************)
(* The OUnit library                                                      *)
(*                                                                        *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           *)
(* Copyright (C) 2010 OCamlCore SARL                                      *)
(* Copyright (C) 2013 Sylvain Le Gall                                     *)
(*                                                                        *)
(* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  *)
(* and Sylvain Le Gall.                                                   *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this document and the OUnit software ("the Software"), to    *)
(* deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute,           *)
(* sublicense, and/or sell copies of the Software, and to permit persons  *)
(* to whom the Software is furnished to do so, subject to the following   *)
(* conditions:                                                            *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* The Software is provided ``as is'', without warranty of any kind,      *)
(* express or implied, including but not limited to the warranties of     *)
(* merchantability, fitness for a particular purpose and noninfringement. *)
(* In no event shall Maas-Maarten Zeeman be liable for any claim, damages *)
(* or other liability, whether in an action of contract, tort or          *)
(* otherwise, arising from, out of or in connection with the Software or  *)
(* the use or other dealings in the software.                             *)
(*                                                                        *)
(* See LICENSE.txt for details.                                           *)
(**************************************************************************)

open OUnitTest
open OUnit2

let test_assert_raises _ =
  assert_raises
    (OUnit_failure "expected: Failure(\"Boo\") but got: Failure(\"Foo\")")
    (fun _ -> (assert_raises (Failure "Boo")
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises
    (OUnit_failure
       "A label\nexpected: Failure(\"Boo\") but got: Failure(\"Foo\")")
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo")
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises
    (OUnit_failure
       "expected exception Failure(\"Boo\"), \
        but no exception was raised.")
    (fun _ -> (assert_raises (Failure "Boo") (fun _ -> ())));
  assert_raises
    (OUnit_failure "A label\nexpected exception Failure(\"Boo\"), \
                    but no exception was raised.")
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") (fun _ -> ())))

(* Test the float compare, and use the cmp label *)
let test_cmp_float _ =
  assert_equal ~cmp: cmp_float 0.0001 0.0001;
  assert_equal ~cmp: (cmp_float ~epsilon: 0.001) 1.0001 1.00001;
  assert_raises (OUnit_failure "not equal")
      (fun _ -> assert_equal ~cmp: cmp_float 100.0001 101.001)

let test_assert_string _ =
  assert_string "";
  assert_raises (OUnit_failure "A string")
    (fun _ -> assert_string "A string")

let test_assert_bool _ =
  assert_bool "true" true;
  assert_raises (OUnit_failure "false") (fun _ -> assert_bool "false" false)

let test_case_skip _ =
  begin
    try
      skip_if false "test"
    with _ ->
      assert_failure "Should not skip this test."
  end;
  assert_raises (Skip "test") (fun _ -> skip_if true "test")

let test_case_todo _ =
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
