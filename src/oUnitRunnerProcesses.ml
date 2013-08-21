
open OUnitTypes

(* Preliminary version of a process based runner. This is incomplete and using
 * the network to communicate and create process to start slave.
 *)

(* TODO: use fork rather than create_process, this is more resilient to running
 * worker and allows to drop network.
 *)

let base_port = 32757 (* last 5 md5 of "ocaml" *)

(* Run all tests, processus version *)
let run_all_tests logger chooser test_cases =

  let mk_sock () =
    (* we must choose PF_INET as it's the only portable choice *)
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind
      sock
      (Unix.ADDR_INET (Unix.inet_addr_loopback, base_port));
    sock

  and mk_process nb =
    let l = ref [] in
    for i = 0 to nb do
      l := (Unix.create_process
              "oUnitWorker"
              [| (string_of_int base_port) |]
              Unix.stdin
              Unix.stdout
              Unix.stderr) :: !l
    done;
    !l

  (* send a test in one socket *)
  and _send_test (path, test) sock =
    let () =
      OUnitLogger.report logger (TestEvent (path, EStart))
    in
    let data =
      Marshal.to_string (Some (path, test)) [Marshal.Closures]
    in
    if 0 = Unix.write sock data 0 (String.length data) then
      () (* error handling *)
    else
      ()

  (* read a result from one socket then report it *)
  and _recv_test sock =
    let buff = Buffer.create 4096
    and str = ref (String.make 4096 (Char.chr 0))
    in
    while 0 < Unix.read sock !str 0 (String.length !str) do
      Buffer.add_string buff !str;
      str := String.make 4096 (Char.chr 0)
    done;
    let (path, res) =
      Marshal.from_string (Buffer.contents buff) 0
    in
      OUnitLogger.report logger (TestEvent (path, EResult res));
      OUnitLogger.report logger (TestEvent (path, EEnd));
      (path, res)

  in

  let _sock = mk_sock ()
  and _process = mk_process 5
  in
    (* TODO accept on socket, then select on the resulting sockets *)
    []

