
(* Preliminary implementation of threaded version of the runner, see TODO in the
 * code for remaining pieces to fix.
 *)

let thread_pool_threshold =
  OUnitConf.make_int
    "thread_pool_threshold"
    10
    "Under this limit, create exactly one thread by test (threads-runner)."

let thread_pool_size =
  OUnitConf.make_int
    "thread_pool_size"
    15
    "Max number of concurrent threads (threads-runner)."

(* TODO: make logger thread safe.
 * 2 ways to do it:
 * - create a specific logger (fun_logger), that will accumulate data and
 *   merge the results at the end of the test in the main thread that
 *   holds the real logger
 * - put a big lock on the logger to prevent 2 threads to write at the
 *   same time.
 *)

(* Run all test, threaded version *)
let threads_runner conf logger chooser test_cases =

  (* Thread-wide synchronization. *)
  let thread_main (wait_chan, result_chan) =
    while true do
      let event = Event.receive wait_chan in
      let test_case = Event.sync event in
      let res = OUnitRunner.run_one_test conf logger test_case in
        Event.sync (Event.send result_chan res)
    done
  in

  (* Application-wide synchronization, end of perfom_test.runner equivalent *)
  let synchronizer_main (test_number, result_chan, suite_result_chan) =
    let i = ref test_number and l = ref [] in
    while !i > 0 do
      let result_full, other_result_fulls =
        Event.sync (Event.receive result_chan)
      in
        l := (result_full :: other_result_fulls) :: !l;
        decr i
    done;
    Event.sync (Event.send suite_result_chan (List.flatten (List.rev !l)))
  in

  (* Beginning of preform_test.runn equivalent, wait results from synchronizer.
   *)
  let rec schedule wait_chan suite_result_chan = function
    (* TODO: no use of chooser, that is bad. *)
    | [] -> Event.sync (Event.receive suite_result_chan)
    | test::tests_planned ->
        Event.sync (Event.send wait_chan test);
        schedule wait_chan suite_result_chan tests_planned
  in
  (* Init channels to pass values with easy synchronization. *)
  let len = List.length test_cases in

  (* Threads will get tests by there. *)
  let wait_chan = Event.new_channel () in
  (* Threads will send test result here. *)
  let result_chan = Event.new_channel () in

  (* Test results will be aggregated here. *)
  let suite_result_chan = Event.new_channel () in

  (* Init our threads: a pool, a scheduler that dispatches tests, *)
  (* and a synchronizer that aggregates result and call the logger. *)
  let pool_size =
    if len < thread_pool_threshold conf then
      len
    else
      thread_pool_size conf
  in
  let _thrd =
    Thread.create synchronizer_main (len, result_chan, suite_result_chan)
  in
    for i = 0 to pool_size do
      let _thrd =
        Thread.create thread_main (wait_chan, result_chan)
      in
        ()
    done;
    schedule wait_chan suite_result_chan test_cases

let () =
  OUnitRunner.register "threads" 70 threads_runner
