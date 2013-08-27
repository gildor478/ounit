
(** Manipulate the state of OUnit runner.
  *)

open OUnitTypes

type t =
    {
      tests_planned: (path * test_fun) list;
      results: test_results;
    }

let create test_cases =
  {
    results = [];
    tests_planned = test_cases;
  }

let filter_out e lst = List.filter (fun (e', _) -> e <> e') lst

let add_test_result test_result state =
  let (test_path, _, _) = test_result in
    {
      results = test_result :: state.results;
      tests_planned = filter_out test_path state.tests_planned;
      (* TODO: add tests_running. *)
    }

let next_test_case chooser logger state =
  match state.tests_planned with
    | [] ->
        None, state
    | _ ->
        let (test_path, _) as test_case = chooser logger state in
          Some test_case,
          {state with
              (* TODO: add tests_running. *)
               tests_planned = filter_out test_path state.tests_planned}

let get_results state = state.results
