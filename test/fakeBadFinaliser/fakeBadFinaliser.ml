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
 * Fake test, to test finalisation misbehaving.
 *)

open OUnit2


let finaliser_token =
  OUnit2.Conf.make_string "finaliser_token" "abcdef"
    "Token to identify to check in the logs the the finaliser has run."


let finalise_with_error token _ =
  print_endline token;
  Segfault.cause_segfault ()


let suite =
  "OUnitFakeBadFinaliser" >:::
  [
    "test where the finaliser should fail" >::
    (fun test_ctxt ->
       let fake_value = String.make 8 'X' in
       Gc.finalise (finalise_with_error (finaliser_token test_ctxt)) fake_value;
       assert_equal fake_value "XXXXXXXX");
  ]

let () = run_test_tt_main suite
