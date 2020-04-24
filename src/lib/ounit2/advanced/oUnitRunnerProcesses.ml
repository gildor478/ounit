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

(** Use processes to run several tests in parallel.
  *
  * Run processes that handle running tests. The processes read test, execute
  * it, and communicate back to the master the log.
  *
  * This need to be done in another process because OCaml Threads are not truly
  * running in parallel. Moreover we cannot use Unix.fork because it's not
  * portable
  *)

open Unix
open OUnitRunner.GenericWorker


let unix_fork = ref Unix.fork

(* Create functions to handle sending and receiving data over a file descriptor.
 *)
let make_channel
      shard_id
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

  let really_read fd str =
    let off = ref 0 in
    let read = ref 0 in
      while !read < Bytes.length str do
        try
          let one_read =
            Unix.read fd str !off (Bytes.length str - !off)
          in
            read := !read + one_read;
            off := !off + one_read
        with Unix_error(EAGAIN, _, _) ->
          ()
      done;
      str
  in

  let header_str = Bytes.create Marshal.header_size in

  let send_data msg =
    Marshal.to_channel chn_write msg [];
    Stdlib.flush chn_write
  in

  let receive_data () =
    try
      let data_size = Marshal.data_size (really_read fd_read header_str) 0 in
      let data_str = really_read fd_read (Bytes.create data_size) in
      let msg =
        (* TODO: use Marshal.from_bytes when OCaml requirement is > 4.01. *)
        Marshal.from_string
          (Bytes.unsafe_to_string (Bytes.cat header_str data_str))
          0
      in
        msg
    with Failure(msg) ->
      OUnitUtils.failwithf "Communication error with worker processes: %s" msg
  in

  let close () =
    close_out chn_write;
  in
    wrap_channel
      shard_id
      string_of_read_message
      string_of_written_message
      {
        send_data = send_data;
        receive_data = receive_data;
        close = close
      }

let processes_grace_period =
  OUnitConf.make_float
    "processes_grace_period"
    5.0
    "Delay to wait for a process to stop."

let processes_kill_period =
  OUnitConf.make_float
    "processes_kill_period"
    5.0
    "Delay to wait for a process to stop after killing it."

let rec select_no_interrupt read_descrs write_descrs except_descrs timeout =
  if timeout < 0.0 then begin
    [], [], []
  end else begin
    try
      Unix.select read_descrs write_descrs except_descrs 0.1
    with Unix.Unix_error (Unix.EINTR, "select", "") ->
      select_no_interrupt
        read_descrs write_descrs except_descrs (timeout -. 0.1)
  end

let create_worker ~shard_id ~master_id ~worker_log_file conf map_test_cases =
  let safe_close fd = try close fd with Unix_error _ -> () in
  let pipe_read_from_worker, pipe_write_to_master = Unix.pipe () in
  let pipe_read_from_master, pipe_write_to_worker  = Unix.pipe () in
  match !unix_fork () with
    | 0 ->
        (* Child process. *)
        let () =
          safe_close pipe_read_from_worker;
          safe_close pipe_write_to_worker;
          (* stdin/stdout/stderr remain open and shared with master. *)
          ()
        in
        let channel =
          make_channel
            shard_id
            string_of_message_to_worker
            string_of_message_from_worker
            pipe_read_from_master
            pipe_write_to_master
        in
          main_worker_loop
            conf
            ~yield:ignore
            channel
            ~shard_id
            map_test_cases
            ~worker_log_file;
          channel.close ();
          safe_close pipe_read_from_master;
          safe_close pipe_write_to_master;
          exit 0

    | pid ->
        let channel =
          make_channel
            master_id
            string_of_message_from_worker
            string_of_message_to_worker
            pipe_read_from_worker
            pipe_write_to_worker
        in

        let rstatus = ref None in

        let msg_of_process_status status =
          if status = WEXITED 0 then
            None
          else
            Some (OUnitUtils.string_of_process_status status)
        in

        let is_running () =
          match !rstatus with
            | None ->
                let pid, status = waitpid [WNOHANG] pid in
                  if pid <> 0 then begin
                    rstatus := Some status;
                    false
                  end else begin
                    true
                  end
            | Some _ ->
                false
        in

        let close_worker () =
          let rec wait_end timeout =
            if timeout < 0.0 then begin
              false, None
            end else begin
              if is_running () then
                let _, _, _ = select_no_interrupt [] [] [] 0.1 in
                wait_end (timeout -. 0.1)
              else
                  match !rstatus with
                  | Some status -> true, msg_of_process_status status
                  | None -> true, None
            end
          in

          let ended, msg_opt =
            channel.close ();
            safe_close pipe_read_from_worker;
            safe_close pipe_write_to_worker;
            (* Recovery for worker going wild and not dying. *)
            List.fold_left
              (fun (ended, msg_opt) signal ->
                 if ended then begin
                   ended, msg_opt
                 end else begin
                   kill pid signal;
                   wait_end (processes_kill_period conf)
                 end)
              (wait_end (processes_grace_period conf))
              [15 (* SIGTERM *); 9 (* SIGKILL *)]
          in
            if ended then
              msg_opt
            else
              Some (Printf.sprintf "unable to kill process %d" pid)
        in
          {
            channel = channel;
            close_worker = close_worker;
            select_fd = pipe_read_from_worker;
            shard_id = shard_id;
            is_running = is_running;
          }

(* Filter running workers waiting data. *)
let workers_waiting ~timeout workers =
  let workers_fd_lst =
    List.rev_map (fun worker -> worker.select_fd) workers
  in
  let workers_fd_waiting_lst, _, _ =
    select_no_interrupt workers_fd_lst [] [] timeout
  in
    List.filter
      (fun workers -> List.memq workers.select_fd workers_fd_waiting_lst)
      workers

let init () =
  if Sys.os_type = "Unix" then
    match Sys.backend_type with
    | Native | Bytecode ->
      OUnitRunner.register "processes" 100
        (runner create_worker workers_waiting)
    | Other _ -> ()
