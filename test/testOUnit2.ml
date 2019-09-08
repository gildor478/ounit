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

let test_normal = "Normal" >:: (fun _ -> ())
let test_assert = "Assert" >:: (fun _ -> assert_equal 1 1)
let test_todo = "Todo" >:: (fun _ -> todo "test")
let test_skip = "Skip" >:: (fun _ -> skip_if true "to be skipped")
let test_fail = "Fail" >:: (fun _ -> assert_equal 1 2)
let test_error = "Error" >:: (fun _ -> failwith "Not expected")

let test_ounit2 suite test_ctxt =
  let log_fn, _ = bracket_tmpfile test_ctxt in
  let conf =
    OUnitConf.default
      ~preset:["chooser", "simple";
               "runner", "sequential";
               "output_file", log_fn;
               "display", "false"]
      ()
  in
  let old_get_conf =
    (* TODO: acquire lock *)
    !OUnitCore.run_test_tt_main_conf
  in
  let [@warning "-27"] override_conf ?preset ?argv _ =
    OUnitCore.run_test_tt_main_conf := old_get_conf;
    (* TODO: release lock *)
    conf
  in
  let exit_code = ref 0 in
    OUnitCore.run_test_tt_main_conf := override_conf;
    run_test_tt_main ~exit:(fun i -> exit_code := i) suite;
    !exit_code, log_fn

let test_ok ctxt =
  let exit_code, _ =
    test_ounit2
      ("OK" >:::
       [test_normal;
        test_assert;
        test_skip])
      ctxt
  in
    assert_equal ~printer:string_of_int 0 exit_code

let test_ko =
  let one lst ctxt =
    let exit_code, _ =
      test_ounit2
        ("KO" >:::
         [test_normal;
          test_assert;
          test_skip] @ lst)
        ctxt
    in
      assert_equal ~printer:string_of_int 1 exit_code
  in
    List.map
      (fun lst -> test_case (one lst))
      [
        [test_todo];
        [test_fail];
        [test_error];
        [test_todo; test_fail; test_error];
      ]
(* Construct the test suite *)
let tests =
  "OUnit2" >:::
  [
    "test_ok" >:: test_ok;
    "test_ko" >::: test_ko;
  ]
