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

  let receive_data _ =
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

let create_worker ~shard_id ~master_id ~worker_log_file conf map_test_cases =
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
    let at_end () =
      channel_worker.close ();
      Mutex.lock worker_finished_mutex;
      worker_finished := true;
      Condition.broadcast worker_finished_cond;
      Mutex.unlock worker_finished_mutex
    in
      try
        main_worker_loop
          conf
          ~yield:Thread.yield
          channel_worker
          ~shard_id
          map_test_cases
          ~worker_log_file;
        at_end ()
      with e ->
        at_end ();
        raise e
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

  let is_running () =
    let res =
      Mutex.lock worker_finished_mutex;
      not !worker_finished
    in
      Mutex.unlock worker_finished_mutex;
      res
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
        (* We should kill [thread] here but there seems to be no way to kill a
           thread so we will just fail. *)
        raise (Invalid_argument "Thread.kill not implemented")
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
      is_running = is_running;
    }


let workers_waiting ~timeout:_ workers =
  let channel_timeout = Event.new_channel () in
(* TODO: clean implementation of the timeout.
 * Timeout not implemented, because it should be killed in most cases and
 * actually Thread.kill is not implemented for systhreads.
 * We could do either of this:
 * - Thread.time_read + mkpipe
 * - use signal ALARM
 *
 * Patch welcome.
 *
 * Sylvain Le Gall -- 2013/09/18.
  let thread_timeout =
    Thread.create
      (fun () ->
         Thread.delay timeout;
         Event.sync (Event.send channel_timeout None))
      ()
  in
 *)
  let worker_id_ready =
    Event.select
      (Event.receive channel_timeout
       ::
       (List.rev_map
          (fun worker ->
             Event.wrap
               (Event.receive worker.select_fd)
               (fun s -> Some s))
          workers))
  in
    match worker_id_ready with
      | None ->
(*           Thread.join thread_timeout; *)
          []
      | Some worker_id ->
(*           Thread.kill thread_timeout; *)
          try
            let worker =
              List.find
                (fun worker ->
                   worker.shard_id = worker_id)
                workers
            in
              [worker]
          with Not_found ->
            assert false

let init () =
   OUnitRunner.register "threads" 70 (runner create_worker workers_waiting)
