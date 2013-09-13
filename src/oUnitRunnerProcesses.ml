(** Use processes to run several tests in parallel.
  *
  * Run processes that handle running tests. The processes read test, execute
  * it, and communicate back to the master the log.
  *
  * This need to be done in another process because ocaml Threads are not truly
  * concurrent. Moreover we cannot use Unix.fork because it's not portable
  *)

open OUnitLogger
open OUnitTest
open OUnitState
open Unix

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

(* Turn on to debug communication. *)
let debug_communication = false

(* Create functions to handle sending and receiving data over a file descriptor.
 *)
let make_channel
      id
      string_of_read_message
      string_of_written_message
      fd_read
      fd_write =
  let () =
    set_nonblock fd_read;
    set_close_on_exec fd_read;
    set_close_on_exec fd_write
  in

  let debugf fmt = 
    Printf.ksprintf
      (fun s ->
         if debug_communication then
           prerr_endline ("D("^id^"): "^s))
      fmt
  in

  let chn_write = out_channel_of_descr fd_write in

  let really_read fd str =
    let off = ref 0 in
    let read = ref 0 in
      while !read < String.length str do
        try
          let one_read =
            Unix.read fd str !off (String.length str - !off)
          in
            read := !read + one_read;
            off := !off + one_read
        with Unix_error(EAGAIN, _, _) ->
          ()
      done;
      str
  in

  let header_str = String.create Marshal.header_size in

  let send_data msg =
    debugf "Sending message %S" (string_of_written_message msg);
    Marshal.to_channel chn_write msg [];
    Pervasives.flush chn_write;
    debugf "Message transmitted, continuing."
  in

  let receive_data () =
    let data_size = Marshal.data_size (really_read fd_read header_str) 0 in
    let data_str = really_read fd_read (String.create data_size) in
    let msg = Marshal.from_string (header_str ^ data_str) 0 in
      debugf "Received message %S" (string_of_read_message msg);
      msg
  in

  let close () =
    close_out chn_write;
  in
    {
      send_data = send_data;
      receive_data = receive_data;
      close = close
    }

(* Run a worker, react to message receive from parent. *)
let main_worker_loop conf channel shard_id map_test_cases =
  let logger =
    (* TODO: identify the running process in log. *)
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
          let res = OUnitRunner.run_one_test conf logger test_case in
          channel.send_data (TestDone res);
          loop ()
  in
    loop ()

type worker =
    {
      channel: (message_to_worker, message_from_worker) channel;
      close_worker: unit -> string option;
      select_fd: Unix.file_descr;
      shard_id: string;
    }

let create_worker conf map_test_cases idx =
  let safe_close fd = try close fd with Unix_error _ -> () in
  let pipe_read_from_worker, pipe_write_to_parent = Unix.pipe () in
  let pipe_read_from_parent, pipe_write_to_worker  = Unix.pipe () in
  let shard_id = OUnitUtils.shardf idx in
  match Unix.fork () with
    | 0 ->
        (* Child process. *)
        let () =
          safe_close pipe_read_from_worker;
          safe_close pipe_write_to_worker;
          (* Do we really need to close stdin/stdout? *)
          dup2 pipe_read_from_parent stdin;
          dup2 pipe_write_to_parent stdout;
          (* stderr remains open and shared with parent. *)
          ()
        in
        let channel =
          make_channel
            "worker"
            string_of_message_to_worker
            string_of_message_from_worker
            pipe_read_from_parent
            pipe_write_to_parent
        in
          main_worker_loop conf channel shard_id map_test_cases;
          channel.close ();
          safe_close pipe_read_from_parent;
          safe_close pipe_write_to_parent;
          exit 0

    | pid ->
        let channel =
          make_channel
            "parent"
            string_of_message_from_worker
            string_of_message_to_worker
            pipe_read_from_worker
            pipe_write_to_worker
        in
        let close_worker () =
          channel.close ();
          safe_close pipe_read_from_worker;
          safe_close pipe_write_to_worker;
          (* TODO: recovery for worker going wild and not dying. *)
          match snd(waitpid [] pid) with
            | WEXITED 0 ->
                None
            | _ ->
                (* TODO: better message. *)
                Some "Error"
        in
          {
            channel = channel;
            close_worker = close_worker;
            select_fd = pipe_read_from_worker;
            shard_id = shard_id;
          }

let default_timeout = 5.0

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
    "Number of shards when using 'processes' as a runner."

(* Run all tests. *)
let processes_runner conf logger chooser test_cases =
  let map_test_cases =
    List.fold_left
      (fun mp ((test_path, _) as test_case) ->
         MapPath.add test_path test_case mp)
      MapPath.empty
      test_cases
  in

  let state = OUnitState.create chooser test_cases in

  let shards = max (shards conf) 1 in

  let worker_idx = ref 1 in

  let () = infof logger "Using %d workers maximum." shards in

  let position_of_fake_position = Hashtbl.create 128 in

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
          if OUnitState.worker_number state < shards then begin
            (* Start a worker. *)
            let () = infof logger "Starting worker number %d." !worker_idx; in
            let worker = create_worker conf map_test_cases !worker_idx in
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
          let state =
            List.iter
              (fun worker -> worker.channel.send_data Exit)
              (OUnitState.get_workers state);
            wait_stopped state
          in
            infof logger "Used %d worker during test execution."
              (!worker_idx - 1);
            OUnitState.get_results state

  and wait_stopped state =
    if OUnitState.get_workers state = [] then
      state
    else
      wait_stopped (wait_loop state)

  and wait_loop state =
    let workers = get_workers state in
    let workers_fd_lst =
      List.rev_map (fun worker -> worker.select_fd) workers
    in
    let workers_fd_waiting_lst, _, _ =
      (* TODO: compute expected next timeout *)
      Unix.select workers_fd_lst [] [] default_timeout
    in
    let workers_waiting_lst =
      List.filter
        (fun workers -> List.memq workers.select_fd workers_fd_waiting_lst)
        workers
    in
    let state =
      (* TODO: timeout. *)
      List.fold_left
        (fun state worker ->
           process_message worker (worker.channel.receive_data ()) state)
        state
        workers_waiting_lst
      in
        state

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
           remove_worker worker state

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
               fake_position (OUnitLogger.position logger);
             state
           end
  in
    iter state

let init () =
  if Sys.os_type = "Unix" then
    OUnitRunner.register "processes" 100 processes_runner
