
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
                Scanf.sscanf line "cpu cores : %d" (fun i -> shards := i)
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

let health_check_interval =
  OUnitConf.make_float
    "health_check_interval"
    1.0
    "Seconds between checking health of workers."

(** Build worker based runner. *)
module GenericWorker =
struct
  open OUnitState

  type message_to_worker =
    | Exit
    | RunTest of path

  let string_of_message_to_worker =
    function
      | Exit -> "Exit"
      | RunTest _ -> "RunTest _"

  type message_from_worker =
    | AckExit
    | Log of OUnitTest.log_event_t
    | LogFakePosition of OUnitLogger.position
    | TestDone of (OUnitTest.result_full * OUnitTest.result_list)

  let string_of_message_from_worker =
    function
      | AckExit -> "AckExit"
      | Log _ -> "Log _"
      | LogFakePosition _ -> "LogFakePosition _"
      | TestDone _ -> "TestDone _"

  module MapPath =
    Map.Make
      (struct
         type t = path
         let rec compare lst1 lst2 =
           match lst1, lst2 with
             | hd1 :: tl1, hd2 :: tl2 ->
                 begin
                   match Pervasives.compare hd1 hd2 with
                     | 0 -> compare tl1 tl2
                     | n -> n
                 end
             | [], _ :: _ -> -1
             | _ :: _, [] -> 1
             | [], [] -> 0
       end)

  type ('a, 'b) channel =
      {
        send_data: 'a -> unit;
        receive_data: unit -> 'b;
        close: unit -> unit;
      }

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
  let main_worker_loop conf channel shard_id map_test_cases =
    let logger =
      let base_logger =
        OUnitLogger.fun_logger
          (fun {event = log_ev} -> channel.send_data (Log log_ev))
          ignore
      in

      (* Fake position generator, communicate with master so that
       * master can do the translation of pos.
       *)
      let idx = ref 0 in
      let fpos () =
        let pos =
          incr idx;
          { filename = shard_id; line = !idx }
        in
          channel.send_data (LogFakePosition pos);
          Some pos
      in
        set_shard shard_id {base_logger with fpos = fpos}
    in
    let rec loop () =
      match channel.receive_data () with
        | Exit ->
            channel.send_data AckExit

        | RunTest test_path ->
            let test_case = MapPath.find test_path map_test_cases in
            let res = run_one_test conf logger test_case in
            channel.send_data (TestDone res);
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

  (* Run all tests. *)
  let runner
        create_worker workers_waiting
        conf logger chooser test_cases =
    let map_test_cases =
      List.fold_left
        (fun mp ((test_path, _) as test_case) ->
           MapPath.add test_path test_case mp)
        MapPath.empty
        test_cases
    in

    let state = OUnitState.create chooser test_cases in

    let shards = max (shards conf) 1 in

    let master_id = logger.OUnitLogger.lshard in
    let worker_idx = ref 1 in

    let () = infof logger "Using %d workers maximum." shards in

    let position_of_fake_position = Hashtbl.create 128 in

    let last_health_check = ref (OUnitUtils.now ()) in
    let health_check_interval = health_check_interval conf in
    let need_health_check () =
      let now = OUnitUtils.now () in
      if !last_health_check +. health_check_interval < now then begin
        prerr_endline "health check";
        last_health_check := now;
        true
      end else begin
        false
      end
    in

    let backpatch_log_position (path, result, pos_opt) =
      let real_pos_opt =
        match pos_opt with
          | Some fake_position ->
              begin
                try
                  Hashtbl.find position_of_fake_position fake_position
                with Not_found ->
                  Some fake_position
              end
          | None -> None
      in
        path, result, real_pos_opt
    in

    let rec iter state =
      match OUnitState.next_test_case logger state with
        | Not_enough_worker, state ->
            if OUnitState.count_worker state < shards then begin
              (* Start a worker. *)
              let shard_id = OUnitUtils.shardf !worker_idx in
              let () = infof logger "Starting worker number %s." shard_id in
              let worker =
                create_worker conf map_test_cases shard_id master_id
              in
              let () = infof logger "Worker %s started." worker.shard_id in
              let state = add_worker worker state in
                incr worker_idx;
                iter state
            end else begin
              iter (wait_loop state)
            end

        | Try_again, state ->
            iter (wait_loop state)

        | Next_test_case (test_path, _, worker), state ->
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
                  OUnitState.get_results state
            end else begin
              infof logger "Still %d tests running : %s." count_tests_running
                (String.concat ", "
                   (List.map string_of_path
                      (get_tests_running state)));
              iter (wait_loop state)
            end

    and wait_stopped state =
      if OUnitState.get_workers state = [] then
        state
      else
        wait_stopped (wait_loop state)

    and wait_loop state =
      let state =
        if need_health_check () then
          check_health state
        else
          state
      in
      let () =
        infof logger "Tests running: %s."
          (String.concat ", "
             (List.map string_of_path (get_tests_running state)))
      in
      let workers = get_workers state in
        if workers <> [] then begin
          let workers_waiting_lst = workers_waiting workers in
            (* TODO: timeout. *)
            List.fold_left
              (fun state worker ->
                 process_message worker (worker.channel.receive_data ()) state)
              state
              workers_waiting_lst
        end else begin
          state
        end

    and check_health state =
      let workers_dead =
        infof logger "Checking health of workers.";
        List.filter
          (fun worker -> not (worker.is_running ()))
          (get_workers state)
      in
        if workers_dead <> [] then
          warningf logger "Found %d dead workers." (List.length workers_dead);
        List.fold_left
          (fun state worker_dead ->
             if is_idle_worker worker_dead state then begin
               (* Nevermind. *)
               let epitaph = worker_dead.close_worker () in
                 OUnitUtils.opt (fun msg ->
                                   errorf logger "Idle worker %s is dead: %s"
                                     worker_dead.shard_id msg) epitaph;
                 remove_idle_worker worker_dead state
             end else begin
               (* Argh, a test failed badly! *)
               let test_path = test_path_of_worker worker_dead state in
               let result_msg =
                 match worker_dead.close_worker () with
                   | Some msg ->
                       Printf.sprintf "Worker stops running: %s" msg
                   | None ->
                       "Worker stops running for unknown reason."
               in
               let result = RError (result_msg, None) in
               let log_pos = position logger in
                 report logger (TestEvent (test_path, EResult result));
                 report logger (TestEvent (test_path, EEnd));
                 remove_idle_worker
                   worker_dead
                   (add_test_result
                      ((test_path, result, log_pos), [])
                      worker_dead state)
             end)
          state
          workers_dead

    and process_message worker msg state =
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
             begin
               OUnitLogger.report (set_shard worker.shard_id logger) log_ev;
               state
             end

         | TestDone test_result ->
             OUnitState.add_test_result
               (backpatch_log_position (fst test_result),
                List.map backpatch_log_position (snd test_result))
               worker state

         | LogFakePosition fake_position ->
             begin
               Hashtbl.add position_of_fake_position
                 fake_position (position logger);
               state
             end
    in
      iter state

end
