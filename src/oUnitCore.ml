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

open OUnitUtils
open OUnitTest
open OUnitLogger

(* Plugin initialisation. *)
let () =
  OUnitRunnerProcesses.init ()

(*
 * Types and global states.
 *)

(* Run all tests, report starts, errors, failures, and return the results *)
let perform_test conf logger runner chooser test =
  let rec flatten_test path acc =
    function
      | TestCase(l, f) ->
          (path, l, f) :: acc

      | TestList (tests) ->
          fold_lefti
            (fun acc t cnt ->
               flatten_test
                 ((ListItem cnt)::path)
                 acc t)
            acc tests
      | TestLabel (label, t) ->
          flatten_test ((Label label)::path) acc t
  in
  let test_cases =
    List.rev (flatten_test [] [] test)
  in
    runner conf logger chooser test_cases

(* A simple (currently too simple) text based test runner *)
let run_test_tt conf logger runner chooser test =
  let () =
    Printexc.record_backtrace true
  in

  let () =
    (* TODO: move into perform test. *)
    List.iter
      (fun (k, v) ->
         OUnitLogger.report logger (GlobalEvent (GConf (k, v))))
      (OUnitConf.dump conf)
  in

  (* Now start the test *)
  let running_time, test_results =
    time_fun (perform_test conf logger runner chooser) test
  in

    (* TODO: move into perform test. *)
    (* Print test report *)
    OUnitLogger.report logger
      (GlobalEvent
         (GResults (running_time,
                    test_results,
                    OUnitTest.test_case_count test)));

    (* Reset logger. *)
    OUnitLogger.close logger;

    (* Return the results possibly for further processing *)
    test_results

(* Test-only override. *)
let run_test_tt_main_conf =
  ref (fun ?(preset=[]) ?argv extra_specs ->
         OUnitConf.load
           ?argv
           ~preset:(OUnitChooser.preset (OUnitRunner.preset preset))
           extra_specs)

let suite_name =
  OUnitConf.make_string
    "suite_name"
    "anon"
    "The name of the test suite running."

(* Call this one to act as your main() function. *)
let run_test_tt_main ?(exit=Stdlib.exit) suite =
  let only_test = ref [] in
  let list_test = ref false in
  let extra_specs =
    [
      "-only-test",
      Arg.String (fun str -> only_test := str :: !only_test),
      "path Run only the selected tests.";

      "-list-test",
      Arg.Set list_test,
      " List tests";
    ]
  in
  let preset =
    match suite with
      | OUnitTest.TestLabel (suite_name, _) -> ["suite_name", suite_name]
      | OUnitTest.TestCase _ | OUnitTest.TestList _ -> []
  in
  let conf = !run_test_tt_main_conf ~preset extra_specs in
    if !list_test then
      begin
        List.iter
          (fun pth -> print_endline (OUnitTest.string_of_path pth))
          (OUnitTest.test_case_paths suite)
      end
    else
      begin
        let nsuite =
          if !only_test = [] then
            suite
          else
            begin
              match OUnitTest.test_filter ~skip:true !only_test suite with
                | Some test ->
                    test
                | None ->
                    failwithf
                      "Filtering test %s lead to no tests."
                      (String.concat ", " !only_test)
            end
        in

        let logger =
          OUnitLogger.combine
            [
              OUnitLoggerStd.create conf shard_default;
              OUnitLoggerHTML.create conf;
              OUnitLoggerJUnit.create conf;
              OUnitLoggerCI.create conf;
            ]
        in

        let runner_name, runner = OUnitRunner.choice conf in
        let chooser_name, chooser = OUnitChooser.choice conf in
        let test_results =
          OUnitLogger.infof logger "Runner: %s" runner_name;
          OUnitLogger.infof logger "Chooser: %s" chooser_name;
          run_test_tt conf logger runner chooser nsuite
        in
          if not (OUnitResultSummary.was_successful test_results) then
            exit 1
      end
