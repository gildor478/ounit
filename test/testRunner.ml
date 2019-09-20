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
open TestCommon

let fakeRunner = Conf.make_exec "fakeRunner"
let fakeBadFinaliser = Conf.make_exec "fakeBadFinaliser"

type test_results =
    {
      cases: int;
      tried: int;
      errors: int;
      failures: int;
      skip: int;
      todo: int;
      timeout: int;
    }

let string_of_test_results test_results =
  Printf.sprintf
    "Cases: %d; Tried: %d; Errors: %d; Failures: %d; \
     Skip: %d; Todo: %d; Timeout: %d"
    test_results.cases test_results.tried test_results.errors
    test_results.failures test_results.skip test_results.todo
    test_results.timeout


let run_test_fake_runner ctxt runner args =
  let fn, _ = bracket_tmpfile ctxt in
  let () =
    TestCommonRunner.run_fake_external_prog
      ~ctxt ~exit_code:(Unix.WEXITED 1) ~runner
      (fakeRunner ctxt) args fn
  in

  let mk str =
    let r = ref (-1) in
    let regex =
      Str.regexp (".* I: "^str^": \\([0-9]+\\)\\.$")
    in
      r,
      fun line ->
        if Str.string_match regex line 0 then
          r := int_of_string (Str.matched_group 1 line)
  in

  let cases, fcases = mk "Cases" in
  let tried, ftried = mk "Tried" in
  let errors, ferrors = mk "Errors" in
  let failures, ffailures = mk "Failures" in
  let skip, fskip = mk "Skip" in
  let todo, ftodo = mk "Todo" in
  let timeout, ftimeout = mk "Timeout" in

  let rrunner = ref "" in
  let runner_regex = Str.regexp (".* I: Runner: \\([a-z]+\\)$") in
  let frunner line =
    if Str.string_match runner_regex line 0 then
      rrunner := Str.matched_group 1 line
  in

  let chn = open_in fn in
  let () =
    try
      while true do
        let line = input_line chn in
        List.iter
          (fun f -> f line)
          [frunner; fcases; ftried; ferrors; ffailures; fskip; ftodo; ftimeout]
      done;
    with End_of_file ->
      close_in chn
  in
    assert_equal
      ~msg:"runner"
      ~printer:(fun s -> s)
      runner
      !rrunner;
    assert_bool "Cases initialized." (!cases >= 0);
    assert_bool "Tried initialized." (!tried >= 0);
    assert_bool "Errors initialized." (!errors >= 0);
    assert_bool "Failures initialized." (!failures >= 0);
    assert_bool "Skip initialized." (!skip >= 0);
    assert_bool "Todo initialized." (!todo >= 0);
    assert_bool "Timeout initialized." (!timeout >= 0);
    {
      cases = !cases;
      tried = !tried;
      errors = !errors;
      failures = !failures;
      skip = !skip;
      todo = !todo;
      timeout = !timeout;
    }


let check_standard_results ?(extra_errors=0) ?(extra_timeouts=0) test_results =
  assert_equal
    ~msg:"test results"
    ~printer:string_of_test_results
    {
      cases = 7;
      tried = 7;
      errors = 1 + extra_errors;
      failures = 1;
      skip = 1;
      todo = 1;
      timeout = extra_timeouts;
    }
    test_results

let tests =
  "Runner" >:::
  [
    "Sequential" >::
    (fun ctxt ->
       let test_results = run_test_fake_runner ctxt "sequential" [] in
         check_standard_results test_results);

    "Processes" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" []
       in
         check_standard_results test_results);

    "ProcessesWithBadFinaliser" >::
    (fun ctxt ->
       let () = skip_if_notunix () in
       let finaliser_token = "1234566789" in
       let fn, _ = bracket_tmpfile ctxt in
       let () =
         TestCommonRunner.run_fake_external_prog
           ~ctxt ~runner:"processes" ~exit_code:(Unix.WEXITED 1)
           (fakeBadFinaliser ctxt)
           ["-shards"; "2"; "-finaliser-token"; finaliser_token]
           fn
       in
       let chn = open_in fn in
       let str = Str.regexp (Str.quote finaliser_token) in
       let token_found = ref false in
       try
         while true do
           let line = input_line chn in
           if Str.string_match str line 0 then
             token_found := true
         done;
         assert_bool "Finaliser token found in the logs." !token_found
       with End_of_file ->
         close_in chn);

    "Processes#1" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" ["-shards"; "1"]
       in
         check_standard_results test_results);

    "Processes#2" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" ["-shards"; "2"]
       in
         check_standard_results test_results);

    "Processes+SIGSEGV" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes"
           ["-shards"; "2";
            "-sigsegv" ; "true";
            "-health-check-interval"; "0.0"]
       in
         check_standard_results ~extra_errors:1 test_results);

    "Processes+timeout" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes"
           ["-shards"; "2";
            "-timeout" ; "true";
            "-processes-grace-period"; "0.1"]
       in
         check_standard_results ~extra_timeouts:1 test_results);


    "Threads" >::
    (fun ctxt ->
       let test_results = run_test_fake_runner ctxt "threads" [] in
         check_standard_results test_results);

    "Threads#1" >::
    (fun ctxt ->
       let test_results =
         run_test_fake_runner ctxt "threads" ["-shards"; "1"]
       in
         check_standard_results test_results);

    "Threads#2" >::
    (fun ctxt ->
       let test_results =
         run_test_fake_runner ctxt "threads" ["-shards"; "2"]
       in
         check_standard_results test_results);
  ]
