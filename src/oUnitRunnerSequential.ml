
(* The simplest runner possible, run test one after the other in a single
 * process, without threads.
 *)

open OUnitTypes

(* Run all tests, sequential version *)
let run_all_tests logger chooser test_cases =
  let rec iter state =
    match OUnitState.next_test_case chooser logger state with
      | None, state ->
          OUnitState.get_results state
      | Some test_case, state ->
          iter
            (OUnitState.add_test_result
               (OUnitRunner.run_one_test logger test_case)
               state)
  in
  iter (OUnitState.create test_cases)

let () =
  OUnitRunner.register "sequential" 50 run_all_tests
