(** Use processes to run several tests in parallel.
  *
  * Run processes that handle running tests. The processes read test, execute
  * it, and communicate back to the master the log.
  *
  * This need to be done in another process because ocaml Threads are not truly
  * concurrent. Moreover we cannot use Unix.fork because it's not portable
  *)

open OUnitTypes
open Unix

type message_to_child =
  | Exit
  | RunTest of path
  | LogPosition of position option

let string_of_message_to_child =
  function
    | Exit -> "Exit"
    | RunTest _ -> "RunTest _"
    | LogPosition _ -> "LogPosition _"

type message_from_child =
  | AckExit
  | Log of log_event_t
  | TestDone of test_result_full
  | PositionLog

let string_of_message_from_child =
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
      Marshal.to_channel chn_write msg [];
      Pervasives.flush chn_write
    in
    let acked = really_read fd_read (String.create (String.length ack)) in
      assert(acked = ack);
      ()
  in

  let header_str = String.create Marshal.header_size in

  let receive_data () =
    let data_size = Marshal.data_size (really_read fd_read header_str) 0 in
    let data_str = really_read fd_read (String.create data_size) in
    let msg = Marshal.from_string (header_str ^ data_str) 0 in
      Pervasives.output_string chn_write ack;
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

(* Run a child, react to message receive from parent. *)
let main_child_loop channel map_test_cases =
  let stop = ref false in
  let logger =
    (* TODO: identify the running process in log. *)
    let base_logger =
      OUnitLogger.fun_logger
        (fun {event = log_ev} -> channel.send_data (Log log_ev))
        ignore
    in
    let fpos () =
      match base_logger.OUnitLogger.fpos () with
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
      {base_logger with OUnitLogger.fpos = fpos}
  in
    while not !stop do
      match channel.receive_data () with
        | Exit ->
            stop := true
        | RunTest test_path ->
            let test_case = MapPath.find test_path map_test_cases in
            let result = OUnitRunner.run_one_test logger test_case in
              channel.send_data (TestDone result)
        | LogPosition _ ->
            assert false
    done;
    channel.send_data AckExit

type child_state =
  | Idle
  | RunningTest of path
  | Exited

type child =
    {
      channel: (message_to_child, message_from_child) channel;
      close_child: unit -> string option;
      select_fd: Unix.file_descr;
      mutable state: child_state;
    }

let create_child map_test_cases =
  let safe_close fd = try close fd with Unix_error _ -> () in
  let pipe_read_from_child, pipe_write_to_parent = Unix.pipe () in
  let pipe_read_from_parent, pipe_write_to_child  = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
        (* Child process. *)
        let () =
          safe_close pipe_read_from_child;
          safe_close pipe_write_to_child;
          (* Do we really need to close stdin/stdout? *)
          dup2 pipe_read_from_parent stdin;
          dup2 pipe_write_to_parent stdout;
          (* stderr remains open and shared with parent. *)
          ()
        in
        let channel =
          make_channel
            "child"
            string_of_message_to_child
            string_of_message_from_child
            pipe_read_from_parent
            pipe_write_to_parent
        in
          main_child_loop channel map_test_cases;
          channel.close ();
          safe_close pipe_read_from_parent;
          safe_close pipe_write_to_parent;
          exit 0

    | pid ->
        let channel =
          make_channel
            "parent"
            string_of_message_from_child
            string_of_message_to_child
            pipe_read_from_child
            pipe_write_to_child
        in
        let close_child () =
          channel.close ();
          safe_close pipe_read_from_child;
          safe_close pipe_write_to_child;
          (* TODO: recovery for child going wild and not dying. *)
          match snd(waitpid [] pid) with
            | WEXITED 0 ->
                None
            | _ ->
                (* TODO: better message. *)
                Some "Error"
        in
          {
            channel = channel;
            close_child = close_child;
            select_fd = pipe_read_from_child;
            state = Idle;
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
  OUnitConf.make
    "shards"
    ~arg_string:"n"
    ~printer:string_of_int
    (fun r -> Arg.Set_int r)
    !shards
    "Number of shards when using 'processes' as a runner."

(* Run all tests. *)
let run_all_tests logger chooser test_cases =
  let map_test_cases =
    List.fold_left
      (fun mp ((test_path, _) as test_case) ->
         MapPath.add test_path test_case mp)
      MapPath.empty
      test_cases
  in

  let children =
    Array.to_list
      (Array.init
         (shards ())
         (fun n -> create_child map_test_cases))
  in

  let filter_out e lst = List.filter (fun (e', _) -> e <> e') lst in

  let add_test_result test_result state =
    let (test_path, _, _) = test_result in
      {
        results = test_result :: state.results;
        tests_planned = filter_out test_path state.tests_planned;
        (* TODO: add tests_running. *)
      }
  in

  let next_test_case state =
    match state.tests_planned with
      | [] ->
          None, state
      | _ ->
          let (test_path, _) as test_case = chooser logger state in
            Some test_case,
            {state with
                (* TODO: add tests_running. *)
                 tests_planned = filter_out test_path state.tests_planned}
  in

  let state =
    {
      results = [];
      tests_planned = test_cases;
    }
  in

  (* Initial assignement of test to children. *)
  let state =
    List.fold_left
      (fun state child ->
         match next_test_case state with
           | Some (test_path, _), state ->
               child.channel.send_data (RunTest test_path);
               child.state <- RunningTest test_path;
               state
           | None, state ->
               child.channel.send_data Exit;
               child.state <- Idle;
               state)
      state
      children
  in

  let rec iter state =
    if List.for_all (fun child -> child.state = Exited) children then begin
      state.results
    end else begin
      let children_fd_lst =
        List.rev_map (fun worker -> worker.select_fd) children
      in
      let children_fd_waiting_lst, _, _ =
        (* TODO: compute expected next timeout *)
        Unix.select children_fd_lst [] [] default_timeout
      in
      let children_waiting_lst =
        List.filter
          (fun children -> List.memq children.select_fd children_fd_waiting_lst)
          children
      in
      let state =
        (* TODO: timeout. *)
        List.fold_left
          (fun state child ->
             match child.channel.receive_data () with
               | AckExit ->
                   child.state <- Exited;
                   state
               | Log log_ev ->
                   OUnitLogger.report logger log_ev;
                   state
               | TestDone test_result ->
                   begin
                     let next_test_case_opt, state =
                       next_test_case (add_test_result test_result state)
                     in
                     let () =
                       match next_test_case_opt with
                         | Some (test_path, _) ->
                             child.channel.send_data (RunTest test_path);
                             child.state <- RunningTest test_path
                         | None ->
                             child.channel.send_data Exit;
                             child.state <- Idle
                     in
                       state
                   end
               | PositionLog ->
                   child.channel.send_data
                     (LogPosition (OUnitLogger.position logger));
                   state)
          state
          children_waiting_lst
      in
        iter state
    end
  in

    iter state

let () =
  if Sys.os_type = "Unix" then
    OUnitRunner.register "processes" 100 run_all_tests
