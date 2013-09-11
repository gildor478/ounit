
(** Manipulate the state of OUnit runner.
  *)

open OUnitTest

type 'worker next_test_case_t =
  | Not_enough_worker
  | Try_again
  | Next_test_case of path * test_fun * 'worker
  | Finished

type 'worker t =
    {
      tests_planned: (path * test_fun) list;
      tests_running: (path * 'worker) list;
      idle_workers: 'worker list;
      results: OUnitTest.result_list;
      chooser: OUnitChooser.chooser;
    }

let create chooser test_cases =
  {
    results = [];
    tests_planned = test_cases;
    tests_running = [];
    idle_workers = [];
    chooser = chooser;
  }

let filter_out e lst = List.filter (fun (e', _) -> e <> e') lst

let add_test_result (test_result, other_test_results) worker state =
  let (test_path, _, _) = test_result in
    {
      state with
        results = (test_result :: other_test_results) @ state.results;
        tests_planned = filter_out test_path state.tests_planned;
        tests_running = filter_out test_path state.tests_running;
        idle_workers = worker :: state.idle_workers;
    }

let add_worker worker state =
  {state with idle_workers = worker :: state.idle_workers}

let remove_worker worker state =
  let found, idle_workers =
    List.fold_left
      (fun (found, lst) worker' ->
         if worker' == worker then
           true, lst
         else
           found, worker' :: lst)
      (false, [])
      state.idle_workers
  in
    if not found then
      raise Not_found;
    {state with idle_workers = idle_workers}

let worker_number state =
  List.length state.idle_workers + List.length state.tests_running

let get_workers state =
  List.rev_append state.idle_workers (List.rev_map snd state.tests_running)

let next_test_case logger state =
  match state.tests_planned, state.idle_workers with
    | [], _ ->
        Finished, state
    | _, worker :: tl_workers ->
        begin
          let choice =
            state.chooser
              logger (List.map fst state.tests_planned) state.results
          in
          match choice with
            | Some test_path ->
                begin
                  try
                    let test_fun = List.assoc test_path state.tests_planned in
                    Next_test_case (test_path, test_fun, worker),
                    {state with
                         tests_running =
                           (test_path, worker) :: state.tests_running;
                         tests_planned =
                           filter_out test_path state.tests_planned;
                         idle_workers =
                           tl_workers}
                  with Not_found ->
                    assert false
                end
            | None ->
                Try_again, state
        end
    | _, [] ->
        Not_enough_worker, state

let get_results state = state.results
