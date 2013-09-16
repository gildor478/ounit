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
open OUnitRunner.GenericWorker

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
    Marshal.to_channel chn_write msg [];
    Pervasives.flush chn_write
  in

  let receive_data () =
    let data_size = Marshal.data_size (really_read fd_read header_str) 0 in
    let data_str = really_read fd_read (String.create data_size) in
    let msg = Marshal.from_string (header_str ^ data_str) 0 in
      msg
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

let create_worker conf map_test_cases shard_id master_id =
  let safe_close fd = try close fd with Unix_error _ -> () in
  let pipe_read_from_worker, pipe_write_to_master = Unix.pipe () in
  let pipe_read_from_master, pipe_write_to_worker  = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
        (* Child process. *)
        let () =
          safe_close pipe_read_from_worker;
          safe_close pipe_write_to_worker;
          (* Do we really need to close stdin/stdout? *)
          dup2 pipe_read_from_master stdin;
          dup2 pipe_write_to_master stdout;
          (* stderr remains open and shared with master. *)
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
            conf channel shard_id map_test_cases;
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
        let close_worker () =
          let rec wait_end timeout =
            if timeout < 0.0 then begin
              false, None
            end else begin
              let pid, status = waitpid [WNOHANG] pid in
              if pid = 0 then begin
                (* Wait 0.1 seconds and continue. *)
                let _, _, _ = Unix.select [] [] [] 0.1 in
                  wait_end (timeout -. 0.1)
              end else begin
                let msg =
                  match status with
                    | WEXITED 0 ->
                        None
                    | WEXITED n ->
                        Some (Printf.sprintf "exited with code %d" n)
                    | WSIGNALED n ->
                        Some (Printf.sprintf "killed by signal %d" n)
                    | WSTOPPED n ->
                        Some (Printf.sprintf "stopped by signal %d" n)
                in
                  true, msg
              end
            end
          in

          let ended, res =
            channel.close ();
            safe_close pipe_read_from_worker;
            safe_close pipe_write_to_worker;
            (* Recovery for worker going wild and not dying. *)
            List.fold_left
              (fun (ended, res) signal ->
                 if ended then begin
                   ended, res
                 end else begin
                   kill pid signal;
                   wait_end 5.0
                 end)
              (wait_end 5.0)
              [15 (* SIGTERM *); 9 (* SIGKILL *)]
          in
            if ended then
              res
            else
              Some (Printf.sprintf "unable to kill process %d" pid)
        in
          {
            channel = channel;
            close_worker = close_worker;
            select_fd = pipe_read_from_worker;
            shard_id = shard_id;
          }

let default_timeout = 5.0

(* Filter running workers waiting data. *)
let workers_waiting workers =
  let workers_fd_lst =
    List.rev_map (fun worker -> worker.select_fd) workers
  in
  let workers_fd_waiting_lst, _, _ =
    (* TODO: compute expected next timeout *)
    Unix.select workers_fd_lst [] [] default_timeout
  in
    List.filter
      (fun workers -> List.memq workers.select_fd workers_fd_waiting_lst)
      workers

let init () =
  if Sys.os_type = "Unix" then
    OUnitRunner.register "processes" 100
      (runner create_worker workers_waiting)
