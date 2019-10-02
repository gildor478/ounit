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

open OUnitTest
open OUnitLogger

(** Number of shards to use. The way the shards are used depends on the type of
    runner.
  *)
let shards =
  let shards = ref 2 in
  if Sys.os_type = "Unix" then begin
    if Sys.file_exists "/proc/cpuinfo" then begin
      let chn_in = open_in "/proc/cpuinfo" in
      let () =
        try
          while true do
            try
              let line = input_line chn_in in
              Scanf.sscanf line "cpu cores : %d" (fun i -> shards := max i 2)
            with Scanf.Scan_failure _ ->
              ()
          done
        with End_of_file ->
          ()
      in
        close_in chn_in
    end
  end;
  OUnitConf.make_int
    "shards"
    !shards
    "Number of shards to use as worker (threads or processes)."

(** Whether or not run a Gc.full_major in between tests. This adds time
    when running tests, but helps to avoid unexpected error due to finalisation
    of values allocated during a test.
  *)
let run_gc_full_major =
  OUnitConf.make_bool
    "run_gc_full_major" true
    "Run a Gc.full_major in between tests."

(** Common utilities to run test. *)
let run_one_test conf logger shared test_path (test_fun: OUnitTest.test_fun) =
  let () = OUnitLogger.report logger (TestEvent (test_path, EStart)) in
  let non_fatal = ref [] in
  let main_result_full =
    with_ctxt conf logger shared non_fatal test_path
      (fun ctxt ->
         let check_env = OUnitCheckEnv.create () in
         let result_full =
           try
             test_fun ctxt;
             OUnitCheckEnv.check ctxt check_env;
             if run_gc_full_major conf then begin
               Gc.full_major ();
             end;
             test_path, RSuccess, None
           with e ->
             OUnitTest.result_full_of_exception ctxt e
         in
         report_result_full ctxt result_full)
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
    OUnitLogger.report logger (TestEvent (test_path, EEnd));
    result_full, other_result_fulls

type runner =
    OUnitConf.conf ->
    OUnitTest.logger ->
    OUnitChooser.chooser ->
    (path * test_length * test_fun) list ->
    OUnitTest.result_list

(* The simplest runner possible, run test one after the other in a single
 * process, without threads.
 *)

(* Run all tests, sequential version *)
let sequential_runner: runner = fun conf logger chooser test_cases ->
  let shared = OUnitShared.create () in
  let rec iter state =
    match OUnitState.next_test_case conf logger state with
      | OUnitState.Finished, state ->
          OUnitState.get_results state
      | OUnitState.Next_test_case (test_path, test_fun, worker), state ->
          iter
            (OUnitState.test_finished conf
               (run_one_test conf logger shared test_path test_fun)
               worker state)
      | (OUnitState.Try_again | OUnitState.Not_enough_worker), _ ->
          assert false
  in
  let state =
    OUnitState.add_worker () (OUnitState.create conf chooser test_cases)
  in
  iter state

(**/**)
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
(**/**)

include Plugin


(** Build worker based runner. *)
module GenericWorker =
struct
  open OUnitState

  type message_to_worker =
    | Exit
    | AckLock of bool
    | RunTest of path

  let string_of_message_to_worker =
    function
      | Exit -> "Exit"
      | AckLock _ -> "AckLock _"
      | RunTest _ -> "RunTest _"

  type message_from_worker =
    | AckExit
    | Log of OUnitTest.log_event_t
    | Lock of int
    | Unlock of int
    | TestDone of (OUnitTest.result_full * OUnitTest.result_list)

  let string_of_message_from_worker =
    function
      | AckExit -> "AckExit"
      | Log _ -> "Log _"
      | Lock _ -> "Lock _"
      | Unlock _ -> "Unlock _"
      | TestDone _ -> "TestDone _"

  module MapPath =
    Map.Make
      (struct
         type t = path
         let rec compare lst1 lst2 =
           match lst1, lst2 with
             | hd1 :: tl1, hd2 :: tl2 ->
                 begin
                   match Stdlib.compare hd1 hd2 with
                     | 0 -> compare tl1 tl2
                     | n -> n
                 end
             | [], _ :: _ -> -1
             | _ :: _, [] -> 1
             | [], [] -> 0
       end)


  type map_test_cases =
    (OUnitTest.path * OUnitTest.test_length * (OUnitTest.ctxt -> unit)) MapPath.t

  type ('a, 'b) channel =
      {
        send_data: 'a -> unit;
        receive_data: unit -> 'b;
        close: unit -> unit;
      }

  type worker_channel = (message_from_worker, message_to_worker) channel

  (* Add some extra feature to channel. *)
  let wrap_channel
        shard_id
        string_of_read_message
        string_of_written_message
        channel =
    (* Turn on to debug communication in channel. *)
    let debug_communication = false in
      if debug_communication then begin
        let debugf fmt =
          Printf.ksprintf
            (fun s ->
               if debug_communication then
                 prerr_endline ("D("^shard_id^"): "^s))
            fmt
        in
        let send_data msg =
          debugf "Sending message %S" (string_of_written_message msg);
          channel.send_data msg;
          debugf "Message transmitted, continuing."
        in

        let receive_data () =
          let () = debugf "Waiting to receive data." in
          let msg = channel.receive_data () in
            debugf "Received message %S" (string_of_read_message msg);
            msg
        in
        {
          send_data = send_data;
          receive_data = receive_data;
          close = channel.close;
        }
      end else begin
        channel
      end


  (* Run a worker, react to message receive from parent. *)
  let main_worker_loop
        ~yield
        ~shard_id
        ~worker_log_file
        (conf: OUnitConf.conf)
        (channel: worker_channel)
        (map_test_cases: map_test_cases) =
    let logger =
      let master_logger =
        set_shard shard_id
          (OUnitLogger.fun_logger
             (fun {event = log_ev; _} -> channel.send_data (Log log_ev))
             ignore)
      in
      let base_logger =
        if worker_log_file then
          OUnitLoggerStd.create_file_logger conf shard_id
        else
          OUnitLogger.null_logger
      in
        OUnitLogger.combine [base_logger; master_logger]
    in

    let shared =
      let try_lock id =
        channel.send_data (Lock id);
        match channel.receive_data () with
          | AckLock b ->
             b
          | Exit | RunTest _ ->
              assert false
      in
      let rec lock id =
        if not (try_lock id) then begin
          yield ();
          lock id
        end else begin
          ()
        end
      in
      let unlock id =
        channel.send_data (Unlock id);
      in
      let global =
        {
          OUnitShared.
          lock = lock;
          try_lock = try_lock;
          unlock = unlock;
        }
      in
        {
          OUnitShared.
          global = global;
          process = OUnitShared.noscope_create ();
        }
    in

    let rec loop () =
      match channel.receive_data () with
        | Exit ->
            channel.send_data AckExit

        | RunTest test_path ->
            let test_path, _, test_fun =
              MapPath.find test_path map_test_cases
            in
            let res = run_one_test conf logger shared test_path test_fun in
            channel.send_data (TestDone res);
            loop ()

        | AckLock _ ->
            loop ()
    in
      loop ()

  type 'a worker =
      {
        channel: (message_to_worker, message_from_worker) channel;
        close_worker: unit -> string option;
        select_fd: 'a;
        shard_id: string;
        is_running: unit -> bool;
      }

  type 'a worker_creator =
    shard_id:string -> master_id:string -> worker_log_file:bool ->
    OUnitConf.conf -> map_test_cases -> 'a worker

  type 'a workers_waiting_selector =
    timeout:float -> 'a worker list -> 'a worker list

  (* Run all tests. *)
  let runner
        (create_worker: 'a worker_creator)
        (workers_waiting: 'a workers_waiting_selector) : runner =
    fun (conf: OUnitConf.conf) logger chooser test_cases ->
    let map_test_cases =
      List.fold_left
        (fun mp ((test_path, _, _) as test_case) ->
           MapPath.add test_path test_case mp)
        MapPath.empty
        test_cases
    in

    let state = OUnitState.create conf chooser test_cases in

    let shards = max (shards conf) 1 in

    let master_id = logger.OUnitLogger.lshard in

    let worker_idx = ref 1 in

    let test_per_worker, incr_tests_per_worker =
      OUnitUtils.make_counter ()
    in
    let health_check_per_worker, incr_health_check_per_worker =
      OUnitUtils.make_counter ()
    in

    let () = infof logger "Using %d workers maximum." shards; in

    let worker_log_file =
      if not (OUnitLoggerStd.is_output_file_shard_dependent conf) then begin
        warningf logger
          "-output-file doesn't include $(shard_id), \
           shards won't have file log.";
        false
      end else begin
        true
      end
    in

    let master_shared = OUnitShared.noscope_create () in

    (* Act depending on the received message. *)
    let process_message worker msg state =
       match msg with
         | AckExit ->
             let msg_opt =
               infof logger "Worker %s has ended." worker.shard_id;
               worker.close_worker ()
             in
             OUnitUtils.opt
               (errorf logger "Worker return status: %s")
               msg_opt;
             remove_idle_worker worker state

         | Log log_ev ->
             OUnitLogger.report (set_shard worker.shard_id logger) log_ev;
             state

         | Lock id ->
             worker.channel.send_data
               (AckLock (master_shared.OUnitShared.try_lock id));
             state

         | Unlock id ->
             master_shared.OUnitShared.unlock id;
             state

         | TestDone test_result ->
             OUnitState.test_finished conf test_result worker state
    in

    (* Report a worker dead and unregister it. *)
    let declare_dead_worker test_path worker result state =
       let log_pos = position logger in
         report logger (TestEvent (test_path, EResult result));
         report logger (TestEvent (test_path, EEnd));
         remove_idle_worker
           worker
           (test_finished conf
              ((test_path, result, log_pos), [])
              worker state)
    in

    let declare_dead_idle_worker worker state =
      let msg =
        Printf.sprintf "Worker %s died unexpectedly." worker.shard_id
      in
      report logger (GlobalEvent (GLog (`Info, msg)));
      remove_idle_worker worker state
    in

    (* Kill the worker that has timed out. *)
    let kill_timeout state =
      List.fold_left
        (fun state (test_path, test_length, worker) ->
           let _msg : string option =
             errorf logger "Worker %s, running test %s has timed out."
               worker.shard_id (string_of_path test_path);
             worker.close_worker ()
           in
             declare_dead_worker test_path worker (RTimeout test_length) state)
        state
        (get_worker_timed_out state)
    in

    (* Check that worker are healthy (i.e. still running). *)
    let check_health state =
      List.fold_left
        (fun state (test_path_opt, worker) ->
           incr_health_check_per_worker worker.shard_id;
           if worker.is_running () then begin
             match test_path_opt with
             | Some test_path -> update_test_activity test_path state
             | None -> state
           end else begin
             match test_path_opt with
             | Some test_path ->
               begin
                 (* Argh, a test failed badly! *)
                 let result_msg =
                   errorf logger
                     "Worker %s, running test %s is not running anymore."
                     worker.shard_id (string_of_path test_path);
                   match worker.close_worker () with
                   | Some msg ->
                     Printf.sprintf "Worker stops running: %s" msg
                   | None ->
                     "Worker stops running for unknown reason."
                 in
                   declare_dead_worker test_path worker
                     (RError (result_msg, None))
                     state
               end
             | None ->
               declare_dead_idle_worker worker state
           end)
        state
        (get_worker_need_health_check state)
    in

    (* Main wait loop. *)
    let wait_test_done state =
      let state = (check_health (kill_timeout state)) in
      if get_workers state <> [] then begin
        let workers_waiting_lst =
          infof logger "%d tests running: %s."
            (count_tests_running state)
            (String.concat ", "
               (List.map string_of_path (get_tests_running state)));
          workers_waiting ~timeout:(timeout state) (get_workers state) 
        in
          List.fold_left
            (fun state worker ->
               process_message worker (worker.channel.receive_data ()) state)
            state
            workers_waiting_lst

      end else begin
        state
      end
    in

    (* Wait for every worker to stop. *)
    let rec wait_stopped state =
      if OUnitState.get_workers state = [] then
        state
      else
        wait_stopped (wait_test_done state)
    in

    let rec iter state =
      match OUnitState.next_test_case conf logger state with
        | Not_enough_worker, state ->
            if OUnitState.count_worker state < shards then begin
              (* Start a worker. *)
              let shard_id = OUnitUtils.shardf !worker_idx in
              let () = infof logger "Starting worker number %s." shard_id in
              let worker =
                create_worker
                  ~shard_id ~master_id ~worker_log_file conf map_test_cases
              in
              let () = infof logger "Worker %s started." worker.shard_id in
              let state = add_worker worker state in
                incr worker_idx;
                iter state
            end else begin
              iter (wait_test_done state)
            end

        | Try_again, state ->
            iter (wait_test_done state)

        | Next_test_case (test_path, _, worker), state ->
            incr_tests_per_worker worker.shard_id;
            worker.channel.send_data (RunTest test_path);
            iter state

        | Finished, state ->
            let count_tests_running = OUnitState.count_tests_running state in
            if count_tests_running = 0 then begin
              let state =
                List.iter
                  (fun worker -> worker.channel.send_data Exit)
                  (OUnitState.get_workers state);
                  wait_stopped state
                in
                  infof logger "Used %d worker during test execution."
                    (!worker_idx - 1);
                  List.iter
                    (fun (shard_id, count) ->
                       infof logger "Run %d tests with shard %s."
                         count shard_id)
                    (test_per_worker ());
                  List.iter
                    (fun (shard_id, count) ->
                       infof logger "Check health of shard %s, %d times."
                         shard_id count)
                    (health_check_per_worker ());
                  OUnitState.get_results state
            end else begin
              infof logger "Still %d tests running : %s." count_tests_running
                (String.concat ", "
                   (List.map string_of_path
                      (get_tests_running state)));
              iter (wait_test_done state)
            end
    in
      iter state

end
