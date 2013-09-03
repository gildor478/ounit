(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008                  *)
(* Maas-Maarten Zeeman.                                                *)
(* Copyright 2010 OCamlCore SARL                                       *)
(* All rights reserved. See LICENCE for details.                       *)
(***********************************************************************)

open OUnit2

let test_normal = "Normal" >:: (fun ctxt -> ())
let test_assert = "Assert" >:: (fun ctxt -> assert_equal 1 1)
let test_todo = "Todo" >:: (fun ctxt -> todo "test")
let test_skip = "Skip" >:: (fun ctxt -> skip_if true "to be skipped")
let test_fail = "Fail" >:: (fun ctxt -> assert_equal 1 2)
let test_error = "Error" >:: (fun ctxt -> failwith "Not expected")

let bracket_test_ounit2 suite f =
  bracket_tmpfile
    (fun (ctxt, (log_fn, _)) ->
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
       let override_conf ?preset ?argv extra_specs =
         OUnitCore.run_test_tt_main_conf := old_get_conf;
         (* TODO: release lock *)
         conf
       in
       let exit_code = ref 0 in
         OUnitCore.run_test_tt_main_conf := override_conf;
         run_test_tt_main ~exit:(fun i -> exit_code := i) suite;
         f ctxt !exit_code log_fn)

let test_ok ctxt =
  bracket_test_ounit2
    ("OK" >:::
     [test_normal;
      test_assert;
      test_skip])
    (fun ctxt exit_code log_fn ->
       assert_equal
         ~printer:string_of_int
         0
         exit_code)
    ctxt

let test_ko =
  let one lst ctxt =
    bracket_test_ounit2
      ("KO" >:::
       [test_normal;
        test_assert;
        test_skip] @ lst)
      (fun ctxt exit_code log_fn ->
         assert_equal
           ~printer:string_of_int
           1
           exit_code)
      ctxt
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
