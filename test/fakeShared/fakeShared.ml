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
 * Fake tests, to test mutex behavior with different runner.
 *)

open OUnit2
open OUnitShared

let test_mutex ctxt mutex =
  let shared = ctxt.OUnitTest.shared in
    Mutex.lock shared mutex;
    (* On Windows, try_lock will succeed if it has been locked by the thread
     * itself.
     *)
    if Sys.os_type <> "Win32" then
      assert_bool
        "Cannot acquire a locked mutex."
        (not (Mutex.try_lock shared mutex));
    Mutex.unlock shared mutex;
    assert_bool
      "Can acquire an unlocked mutex."
      (Mutex.try_lock shared mutex);
    Mutex.unlock shared mutex

let tests =
  "Shared" >:::
  [
    "MutexGlobal" >::
    (fun ctxt ->
       test_mutex ctxt (Mutex.create ScopeGlobal));

    "MutexProcess" >::
    (fun ctxt ->
       test_mutex ctxt (Mutex.create ScopeProcess));
  ]

let () =
  OUnitThreads.init ();
  run_test_tt_main tests
