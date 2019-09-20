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

(*
 * Fake tests, to test runner behavior in some situation.
 *)

open OUnit2

let sigsegv =
  Conf.make_bool
    "sigsegv"
    false
    "Fail with SIGSEGV."

let timeout =
  Conf.make_bool
    "timeout"
    false
    "Time out."

let suite =
  "TestFakeRunner" >:::
  [
    "success" >::
    (fun _ -> assert_equal 0 0);

    "failure" >::
    (fun _ -> assert_equal 0 1);

    "skip" >::
    (fun _ -> skip_if true "skipped because of me");

    "todo" >::
    (fun _ -> todo "need to make this function");

    "error" >::
    (fun _ -> raise Not_found);

    "SIGSEGV" >::
    (fun ctxt ->
       if sigsegv ctxt then begin
         Segfault.cause_segfault ()
       end);

    "Timeout" >:
    (test_case
       ~length:(OUnitTest.Custom_length 0.1)
       (fun ctxt ->
          if timeout ctxt then
            Unix.sleep 1))
  ]

let () =
  OUnitThreads.init ();
  run_test_tt_main suite
