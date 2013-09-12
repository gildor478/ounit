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
  | LogPosition of position option

let string_of_message_to_worker =
  function
    | Exit -> "Exit"
    | RunTest _ -> "RunTest _"
    | LogPosition _ -> "LogPosition _"

type message_from_worker =
  | AckExit
  | Log of OUnitTest.log_event_t
  | TestDone of (OUnitTest.result_full * OUnitTest.result_list)
  | PositionLog

let string_of_message_from_worker =
  function
    | AckExit -> "AckExit"
    | Log _ -> "Log _"
    | TestDone _ -> "TestDone _"
    | PositionLog -> "PositionLog"

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
let debug_communication = true

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

  let ack = "ACK" in

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

  let send_data msg =
    let () =
      debugf "Sending message %S" (string_of_written_message msg);
      Marshal.to_channel chn_write msg [];
      Pervasives.flush chn_write
    in
    let acked = really_read fd_read (String.create (String.length ack)) in
      debugf "ack read: %S" acked;
      assert(acked = ack);
      ()
  in

  let header_str = String.create Marshal.header_size in

  let receive_data () =
    let data_size = Marshal.data_size (really_read fd_read header_str) 0 in
    let data_str = really_read fd_read (String.create data_size) in
    let msg = Marshal.from_string (header_str ^ data_str) 0 in
      debugf "Received message %S" (string_of_read_message msg);
      Pervasives.output_string chn_write ack;
      debugf "ACK sent.";
      Pervasives.flush chn_write;
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
let main_worker_loop channel conf map_test_cases =
  let stop = ref false in
  let logger =
    (* TODO: identify the running process in log. *)
    let base_logger =
      OUnitLogger.fun_logger
        (fun {event = log_ev} -> channel.send_data (Log log_ev))
        ignore
    in
    let fpos () =
      match base_logger.fpos () with
        | Some _ as e -> e
        | None ->
            begin
              channel.send_data PositionLog;
              match channel.receive_data () with
                | Exit ->
                    stop := true;
                    None
                | RunTest _ ->
                    assert false
                | LogPosition position ->
                    position
            end
    in
      {base_logger with fpos = fpos}
  in
    while not !stop do
      match channel.receive_data () with
        | Exit ->
            stop := true
        | RunTest test_path ->
            let test_case = MapPath.find test_path map_test_cases in
            let res = OUnitRunner.run_one_test conf logger test_case in
              channel.send_data (TestDone res)
        | LogPosition _ ->
            assert false
    done;
    channel.send_data AckExit

type worker =
    {
      channel: (message_to_worker, message_from_worker) channel;
      close_worker: unit -> string option;
      select_fd: Unix.file_descr;
      id: string;
    }

let create_worker conf map_test_cases idx =
  let safe_close fd = try close fd with Unix_error _ -> () in
  let pipe_read_from_worker, pipe_write_to_parent = Unix.pipe () in
  let pipe_read_from_parent, pipe_write_to_worker  = Unix.pipe () in
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
          main_worker_loop channel conf map_test_cases;
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
        let id =
          let fqdn =
            (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name
          in
            Printf.sprintf "%s#%02d" fqdn idx
        in
          {
            channel = channel;
            close_worker = close_worker;
            select_fd = pipe_read_from_worker;
            id = id;
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

  let shards = min (shards conf) 1 in

  let worker_idx = ref 1 in

  let () = infof logger "Using %d workers maximum." shards in

  let rec iter state =
    match OUnitState.next_test_case logger state with
      | Not_enough_worker, state ->
          if OUnitState.worker_number state < shards then begin
            (* Start a worker. *)
            let () = infof logger "Starting worker number %d." !worker_idx; in
            let worker = create_worker conf map_test_cases !worker_idx in
            let () = infof logger "Worker %s started." worker.id in
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
          List.iter
            (fun worker -> worker.channel.send_data Exit)
            (OUnitState.get_workers state);
          wait_stopped state;
          infof logger "Used %d worker during test execution." !worker_idx;
          OUnitState.get_results state

  and wait_stopped state =
    if OUnitState.get_workers state = [] then
      ()
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
           match worker.channel.receive_data () with
             | AckExit ->
                 let msg_opt =
                   infof logger "Worker %s has ended." worker.id;
                   worker.close_worker ()
                 in
                 OUnitUtils.opt
                   (errorf logger "Worker return status: %s")
                   msg_opt;
                 remove_worker worker state
             | Log log_ev ->
                 OUnitLogger.report logger log_ev;
                 state
             | TestDone test_result ->
                 OUnitState.add_test_result test_result worker state
             | PositionLog ->
                 worker.channel.send_data
                   (LogPosition (OUnitLogger.position logger));
                 state)
        state
        workers_waiting_lst
      in
        state
  in
    iter state

let init () =
  if Sys.os_type = "Unix" then
    OUnitRunner.register "processes" 100 processes_runner
