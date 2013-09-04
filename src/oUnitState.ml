
(** Manipulate the state of OUnit runner.
  *)

open OUnitTest

type t =
    {
      tests_planned: (path * test_fun) list;
      tests_running: (path * test_fun) list;
      results: OUnitTest.result_list;
      chooser: t -> (path * test_fun);
    }

let create chooser test_cases =
  {
    results = [];
    tests_planned = test_cases;
    tests_running = [];
    chooser = chooser;
  }

let filter_out e lst = List.filter (fun (e', _) -> e <> e') lst

let add_test_result test_result state =
  let (test_path, _, _) = test_result in
    {
      results = test_result :: state.results;
      tests_planned = filter_out test_path state.tests_planned;
      tests_running = filter_out test_path state.tests_running;
      chooser = state.chooser;
    }

let next_test_case state =
  match state.tests_planned with
    | [] ->
        None, state
    | _ ->
        let (test_path, _) as test_case = state.chooser state in
          Some test_case,
          {state with
               tests_running = test_case :: state.tests_running;
               tests_planned = filter_out test_path state.tests_planned}

let get_results state = state.results
