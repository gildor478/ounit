
(* The simplest runner possible, run test one after the other in a single
 * process, without threads.
 *)

open OUnitTypes

(* Run all tests, sequential version *)
let run_all_tests logger (chooser: chooser) test_cases =
  let rec iter state =
    match state.tests_planned with
      | [] ->
          state.results
      | _ ->
          let (test_path, _) as test_case = chooser state in
          let result = OUnitRunner.run_one_test logger test_case in
            iter
              {
                results = result :: state.results;
                tests_planned =
                  List.filter
                    (fun (test_path', _) -> test_path <> test_path')
                    state.tests_planned
              }
  in
  iter {results = []; tests_planned = test_cases}
