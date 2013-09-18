
(** Manipulate the state of OUnit runner.
  *)

open OUnitTest

type 'worker next_test_case_t =
  | Not_enough_worker
  | Try_again
  | Next_test_case of path * test_fun * 'worker
  | Finished

type time = float

type 'worker test_running =
    {
      test_length: test_length;
      deadline: time;
      next_health_check: time;
      worker: 'worker;
    }

type 'worker t =
    {
      tests_planned: (path * (test_length * test_fun)) list;
      tests_running: (path * ('worker test_running)) list;
      idle_workers: 'worker list;
      results: OUnitTest.result_list;
      chooser: OUnitChooser.chooser;
      health_check_interval: time;
    }

let health_check_interval =
  OUnitConf.make_float
    "health_check_interval"
    1.0
    "Seconds between checking health of workers."

let create conf chooser test_cases =
  {
    results = [];
    tests_planned = List.map
                      (fun (test_path, test_length, test_fun) ->
                         test_path, (test_length, test_fun))
                      test_cases;
    tests_running = [];
    idle_workers = [];
    chooser = chooser;
    health_check_interval = health_check_interval conf;
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

let remove_idle_worker worker state =
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

let count_worker state =
  List.length state.idle_workers + List.length state.tests_running

let count_tests_running state =
  List.length state.tests_running

let get_workers state =
  List.rev_append state.idle_workers
    (List.rev_map (fun (_, {worker = worker}) -> worker)  state.tests_running)

let get_idle_workers state =
  state.idle_workers

let is_idle_worker worker state =
  List.exists (fun worker' -> worker == worker') state.idle_workers

let get_tests_running state =
  List.map fst state.tests_running

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
                    let test_length, test_fun =
                      List.assoc test_path state.tests_planned
                    in
                    let now = OUnitUtils.now () in
                    Next_test_case (test_path, test_fun, worker),
                    {state with
                         tests_running =
                           (test_path,
                            {
                              test_length = test_length;
                              deadline = now +. delay_of_length test_length;
                              next_health_check =
                                now +. state.health_check_interval;
                              worker = worker;
                            }) :: state.tests_running;
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


(** Get all the workers that need to be checked for their health. *)
let get_worker_need_health_check state =
  let now = OUnitUtils.now () in
    List.fold_left
      (fun lst (test_path, test_running) ->
         if test_running.next_health_check <= now then
           (test_path, test_running.worker) :: lst
         else
           lst)
      []
      state.tests_running

(** Update the activity of a worker, this postpone the next health check. *)
let update_test_activity test_path state =
  let now = OUnitUtils.now () in
  let tests_running =
    List.fold_right
      (fun (test_path', test_running) lst ->
         let test_running =
           if test_path' = test_path then
             {test_running with
                  next_health_check = now +. state.health_check_interval}
           else
             test_running
         in
           (test_path', test_running) :: lst)
      state.tests_running
      []
  in
    {state with tests_running = tests_running}

(** Get all the workers that are timed out, i.e. that need to be stopped. *)
let get_worker_timed_out state =
  let now = OUnitUtils.now () in
    List.fold_left
      (fun lst (test_path, test_running) ->
         if test_running.deadline <= now then
           (test_path, test_running.test_length, test_running.worker) :: lst
         else
           lst)
      []
      state.tests_running

(** Compute when is the next time, we should either run health check or timeout
    a test.
 *)
let timeout state =
  let now = OUnitUtils.now () in
  let next_event_time =
    List.fold_left
      (fun next_event_time (_, test_running) ->
         min test_running.next_health_check
           (min test_running.deadline next_event_time))
      (now +. state.health_check_interval)
      state.tests_running
  in
    max 0.0 (next_event_time -. now)
