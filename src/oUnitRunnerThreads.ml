(** Use threads to run several tests concurrently.
  *
  * Run threads that handle running tests. It works the same way
  * as processes. Due to the non-parallel threads behavior in OCaml, you cannot
  * truly use the power of parallelism with threads, except when you have a lot
  * of disk and process operation.
  *)

open OUnitRunner.GenericWorker

let make_channel
      shard_id
      sync_send_data
      (string_of_read_message: 'read -> string)
      (string_of_written_message: 'written -> string)
      (chan_read: 'read Event.channel)
      (chan_write: 'written Event.channel) =
  let chan_sync_send_data = Event.new_channel () in
  let send_data msg =
    if sync_send_data then
      Event.sync (Event.send chan_sync_send_data shard_id);
    Event.sync (Event.send chan_write msg)
  in

  let receive_data msg =
    Event.sync (Event.receive chan_read)
  in
    chan_sync_send_data,
    wrap_channel
      shard_id
      string_of_read_message
      string_of_written_message
      {
        send_data = send_data;
        receive_data = receive_data;
        close = ignore;
      }

let create_worker conf map_test_cases shard_id master_id =
  (* Threads will get message from master by there. *)
  let master_to_worker = Event.new_channel () in
  (* Threads will send message to master by there. *)
  let worker_to_master = Event.new_channel () in
  (* Signal end of the worker. *)
  let worker_finished = ref false in
  let worker_finished_mutex = Mutex.create () in
  let worker_finished_cond = Condition.create () in

  let select_fd, channel_worker =
    make_channel
      shard_id
      true
      string_of_message_to_worker
      string_of_message_from_worker
      master_to_worker
      worker_to_master
  in

  let thread_main_worker () =
      main_worker_loop
        conf channel_worker shard_id map_test_cases;
      channel_worker.close ();
      Mutex.lock worker_finished_mutex;
      worker_finished := true;
      Condition.broadcast worker_finished_cond;
      Mutex.unlock worker_finished_mutex
  in

  let thread = Thread.create thread_main_worker () in

  let _, channel_master =
    make_channel
      master_id
      false
      string_of_message_from_worker
      string_of_message_to_worker
      worker_to_master
      master_to_worker
  in

  let close_worker () =
    let killer () =
      let total_wait = ref 0.0 in
      let step = 0.1 in
      Mutex.lock worker_finished_mutex;
      while !total_wait < 5.0 && not !worker_finished do
        Mutex.unlock worker_finished_mutex;
        Thread.delay step;
        total_wait := !total_wait +. step;
        Mutex.lock worker_finished_mutex
      done;
      if not !worker_finished then begin
        Thread.kill thread;
        worker_finished := true;
        Condition.broadcast worker_finished_cond
      end;
      Mutex.unlock worker_finished_mutex
    in
    let killer_thread = Thread.create killer () in
      Mutex.lock worker_finished_mutex;
      while not !worker_finished do
        Condition.wait worker_finished_cond worker_finished_mutex
      done;
      Mutex.unlock worker_finished_mutex;
      try
        Thread.join killer_thread;
        Thread.join thread;
        None
      with e ->
        Some (Printf.sprintf
                "Exception raised: %s."
                (Printexc.to_string e))
  in
    {
      channel = channel_master;
      close_worker = close_worker;
      select_fd = select_fd;
      shard_id = shard_id;
    }


let workers_waiting workers =
  let worker_id_ready =
    Event.select
      (List.rev_map
         (fun worker -> Event.receive worker.select_fd)
         workers)
  in
    try
      let worker =
        List.find
          (fun worker ->
             worker.shard_id = worker_id_ready)
          workers
      in
        [worker]
    with Not_found ->
      assert false

let init () =
   OUnitRunner.register "threads" 70 (runner create_worker workers_waiting)
