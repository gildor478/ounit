(** OUnitWorker
  *
  * A OUnitWorker is a single process that wait for a socket, read test, execute
  * it, and return the result on the socket
  *
  * This need to be done in another process because ocaml Threads are not truly
  * concurrent. Moreover we cannot use Unix.fork because it's not portable
  *)

open OUnitTypes

let (|>) f x = x f
let (|-) f g x = g (f x)
let tap f x = f x; x

let runner socket =
  let rec wait_socket sock =
    let (socks, _ , _) = (Unix.select [socket] [] [] (-1.)) in
    match socks with
    | [] -> wait_socket sock
    | s_test_cases::l -> s_test_cases

  and read_socket sock =
    let buff = Buffer.create 4096
    and str = ref (String.make 4096 (Char.chr 0))
    in
    while 0 < Unix.read sock !str 0 4096 do
      Buffer.add_string buff !str;
      str := String.make 4096 (Char.chr 0)
    done;
    Buffer.contents buff

  and run_test = function
    | None -> ("", RSuccess)
    | Some (path, test) ->
      try
        test (); (path, RSuccess)
      with e ->
        let backtrace =
          if Printexc.backtrace_status ()
          then Some (Printexc.get_backtrace ())
          else None
        in
        match e with
          | Failure s -> (path, RFailure (s, backtrace))
          | Skip s -> (path, RSkip s)
          | Todo s -> (path, RTodo s)
          | s -> (path, RError (Printexc.to_string s, backtrace))

  and write_socket sock str =
    if 0 = Unix.single_write sock str 0 (String.length str)
    then () (* TODO error handling : exit ? log something ? *)
    else ()

  and quit_if_end = function
    | None -> exit 0
    | Some _ -> ()

  in
  while true do
    wait_socket socket
      |> read_socket
      |> (fun s -> Marshal.from_string s 0)
      |> tap quit_if_end (* if None is send, exit here *)
      |> run_test
      |> (fun res -> Marshal.to_string res [Marshal.Closures])
      |> (write_socket socket)
 done

let _ =
  if Array.length Sys.argv < 2
  then (print_endline "oUnitWorker is only called by OUnit !"; exit 64)
  else
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    int_of_string Sys.argv.(1)
    |> fun s -> (Unix.inet_addr_loopback, s)
    |> fun (a,b) -> Unix.ADDR_INET (a,b)
    |> (Unix.connect sock);
    runner sock
