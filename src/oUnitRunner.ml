
open OUnitTest
open OUnitLogger

(** Common utilities to run test. *)
let run_one_test conf logger test_case =
  let (test_path, test_fun) = test_case in
  let () = OUnitLogger.report logger (TestEvent (test_path, EStart)) in
  let result =
    try
      let () = with_ctxt conf logger test_path test_fun in
        RSuccess
    with e ->
      let backtrace =
        if Printexc.backtrace_status () then
          Some (Printexc.get_backtrace ())
        else
          None
      in
        match e with
          | Failure s -> RFailure (s, backtrace)
          | Skip s -> RSkip s
          | Todo s -> RTodo s
          | s -> RError (Printexc.to_string s, backtrace)
  in
  let position = OUnitLogger.position logger in
    OUnitLogger.report logger (TestEvent (test_path, EResult result));
    OUnitLogger.report logger (TestEvent (test_path, EEnd));
    test_path, result, position

type runner =
    OUnitConf.conf ->
    OUnitTest.logger ->
    OUnitChooser.chooser ->
    (path * test_fun) list ->
    OUnitTest.result_list

(* The simplest runner possible, run test one after the other in a single
 * process, without threads.
 *)

(* Run all tests, sequential version *)
let sequential_runner conf logger chooser test_cases =
  let rec iter state =
    match OUnitState.next_test_case state with
      | None, state ->
          OUnitState.get_results state
      | Some test_case, state ->
          iter
            (OUnitState.add_test_result
               (run_one_test conf logger test_case)
               state)
  in
  iter (OUnitState.create (chooser logger) test_cases)

(* Plugin interface. *)
module Plugin =
  OUnitPlugin.Make
    (struct
       type t = runner
       let name = "runner"
       let conf_help =
         "Select a the method to run tests."
       let default = sequential_runner

     end)

include Plugin

let () =
  register "sequential" 0 sequential_runner

