
open OUnitTest
open OUnitLogger

(** Common utilities to run test. *)
let run_one_test conf logger test_case =
  let (test_path, test_fun) = test_case in
  let () = OUnitLogger.report logger (TestEvent (test_path, EStart)) in
  let non_fatal = ref [] in
  let main_result_full =
    with_ctxt conf logger non_fatal test_path
      (fun ctxt ->
         try
           test_fun ctxt;
           test_path, RSuccess, None
         with e ->
           OUnitTest.result_full_of_exception ctxt e)
  in
  let result_full, other_result_fulls =
    match main_result_full, List.rev !non_fatal with
      | (_, RSuccess, _), [] ->
          main_result_full, []
      | (_, RSuccess, _), hd :: tl ->
          OUnitResultSummary.worst_result_full hd tl
      | _, lst ->
          OUnitResultSummary.worst_result_full main_result_full lst
  in
  let _, result, _ = result_full in
    OUnitLogger.report logger (TestEvent (test_path, EResult result));
    OUnitLogger.report logger (TestEvent (test_path, EEnd));
    result_full, other_result_fulls

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
    match OUnitState.next_test_case logger state with
      | OUnitState.Finished, state ->
          OUnitState.get_results state
      | OUnitState.Next_test_case (test_path, test_fun, worker), state ->
          iter
            (OUnitState.add_test_result
               (run_one_test conf logger (test_path, test_fun))
               worker state)
      | (OUnitState.Try_again | OUnitState.Not_enough_worker), _ ->
          assert false
  in
  let state =
    OUnitState.add_worker () (OUnitState.create chooser test_cases)
  in
  iter state

(* Plugin interface. *)
module Plugin =
  OUnitPlugin.Make
    (struct
       type t = runner
       let name = "runner"
       let conf_help =
         "Select a the method to run tests."
       let default_name = "sequential"
       let default_value = sequential_runner
     end)

include Plugin
