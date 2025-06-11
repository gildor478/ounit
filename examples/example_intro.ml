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

(* The authors recommend opening the module, which brings the operators and
   functions into scope. *)
open OUnit2

let l = [1; 2; 3]
let l2 = [4; 5; 6]

let rec append a b =
  match a with
  | [] -> b
  | h::t -> h::append t b

let rec broken_append a b =
  match a with
  | [] -> []
  | h::t -> h::broken_append t b

(* A simple passing test *)
let test_append _ =
  assert_equal (append l l2) [1; 2; 3; 4; 5; 6];
  assert_equal (append [] l) l;
  assert_bool "message when this test fails" (append l [] = l)
  (* Also assert_string, assert_failure, assert_command etc. *)

(* A simple failing test *)
let test_broken_append _ =
  assert_equal (broken_append l l2) [1; 2; 3; 4; 5; 6]

(* >:: names a test. Then we put them in named groups, for organization with
   >:::. *)
let suite =
  "ExampleTests" >:::
    ["test_append" >:: test_append;
     "test_broken_append" >:: test_broken_append]

(* Run the test suite. *)
let () =
  run_test_tt_main suite
