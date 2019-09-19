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

(** Manipulate the state of OUnit runner.
  *)

open OUnitLogger
open OUnitTest
open OUnitChooser

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
      tests_passed: (OUnitTest.result_full * OUnitTest.result_list) list;
      idle_workers: 'worker list;
      chooser: OUnitChooser.chooser;
      cache: OUnitCache.cache;
      health_check_interval: time;
    }

let health_check_interval =
  OUnitConf.make_float
    "health_check_interval"
    1.0
    "Seconds between checking health of workers."

let create conf chooser test_cases =
  {
    tests_passed = [];
    tests_planned = List.map
                      (fun (test_path, test_length, test_fun) ->
                         test_path, (test_length, test_fun))
                      test_cases;
    tests_running = [];
    idle_workers = [];
    chooser = chooser;
    cache = OUnitCache.load conf;
    health_check_interval = health_check_interval conf;
  }

let filter_out e lst = List.filter (fun (e', _) -> e <> e') lst

let maybe_dump_cache conf state =
  if state.tests_running = [] && state.tests_planned = [] then
    (* We are finished, all results are in, flush the cache. *)
    OUnitCache.dump conf
      (List.fold_left
         (fun cache (path, test_result, _) ->
            OUnitCache.add_result path test_result cache)
         state.cache
         (List.map fst state.tests_passed));
  state

let add_test_results conf all_test_results state =
  let ((test_path, _, _), _) = all_test_results in
  let state =
    {state with
         tests_passed = all_test_results :: state.tests_passed;
         tests_planned = filter_out test_path state.tests_planned};
  in
    maybe_dump_cache conf state

let test_finished conf all_test_results worker state =
  let ((test_path, _, _), _) = all_test_results in
  let state =
    {(add_test_results conf all_test_results state) with
         tests_running = filter_out test_path state.tests_running;
         idle_workers = worker :: state.idle_workers}
  in
    maybe_dump_cache conf state

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
    (List.rev_map (fun (_, {worker = worker; _}) -> worker)  state.tests_running)

let get_idle_workers state =
  state.idle_workers

let is_idle_worker worker state =
  List.exists (fun worker' -> worker == worker') state.idle_workers

let get_tests_running state =
  List.map fst state.tests_running

let rec next_test_case conf logger state =
  match state.tests_planned, state.idle_workers with
    | [], _ ->
        Finished, state
    | _, worker :: tl_workers ->
        begin
          let choice =
            state.chooser
              {
                OUnitChooser.
                tests_planned = List.map fst state.tests_planned;
                tests_running = List.map fst state.tests_running;
                tests_passed = List.map fst state.tests_passed;
                cache = state.cache;
              }
          in
          match choice with
            | Choose test_path ->
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

            | ChooseToPostpone ->
                Try_again, state

            | ChooseToSkip path ->
                let skipped_result = RSkip "Skipped by the chooser." in
                  OUnitLogger.report logger (TestEvent (path, EStart));
                  OUnitLogger.report
                    logger (TestEvent (path, EResult skipped_result));
                  OUnitLogger.report logger (TestEvent (path, EEnd));
                  next_test_case
                    conf logger
                    (add_test_results conf
                       ((path, skipped_result, None), []) state)

            | NoChoice ->
                Finished, state

        end
    | _, [] ->
        Not_enough_worker, state

(** Get all the results. *)
let get_results state =
  List.fold_right
    (fun (result, other_results) res ->
       result :: other_results @ res)
    state.tests_passed []

(** Get all the workers that need to be checked for their health. *)
let get_worker_need_health_check state =
  let now = OUnitUtils.now () in
  let running_workers =
    List.fold_left
      (fun lst (test_path, test_running) ->
         if test_running.next_health_check <= now then
           (Some test_path, test_running.worker) :: lst
         else
           lst)
      []
      state.tests_running
  in
  let idle_workers =
    List.map (fun worker -> (None, worker)) state.idle_workers
  in
  running_workers @ idle_workers

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
    max 0.1 (next_event_time -. now)
