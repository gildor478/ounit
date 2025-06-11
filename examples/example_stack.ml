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

open OUnit2

(*
 * This test shows how brackets can be used. They are handy to create
 * a so called fixture, which can be used for multiple tests
 *)

(* prepare a stack for test *)
let setup _test_ctxt =
  let s = Stack.create () in
  Stack.push 1 s;
  Stack.push 2 s;
  Stack.push 3 s;
  s

let teardown _stack _test_ctxt =
  ()

let test_top stack =
  assert_equal 3 (Stack.top stack)

let test_clear stack =
  Stack.clear stack;
  assert_raises
    Stack.Empty
    (fun _ -> 
       let _i = Stack.top stack in
       ())

let test_pop stack =
  assert_equal 3 (Stack.pop stack);
  assert_equal 2 (Stack.pop stack);
  assert_equal 1 (Stack.pop stack);
  assert_raises
    Stack.Empty
    (fun _ -> let _i : int = Stack.pop stack in ())

let suite =
  "Test Stack" >::: [
    "test_top" >::
    (fun test_ctxt ->
       let stack = bracket setup teardown test_ctxt in
       test_top stack);

    "test_clear" >::
    (fun test_ctxt ->
       let stack = bracket setup teardown test_ctxt in
       test_clear stack);

    "test_pop" >::
    (fun test_ctxt ->
       let stack = bracket setup teardown test_ctxt in
       test_pop stack)
  ]

let () =
  run_test_tt_main suite
