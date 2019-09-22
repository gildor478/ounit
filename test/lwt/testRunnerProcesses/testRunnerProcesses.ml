open OUnit2

let test =
  OUnitTest.TestCase
    (OUnitTest.Short,
     let open Lwt.Infix in
     OUnitLwt.lwt_wrapper
       (fun _ctxt ->
          Lwt_io.open_file ~mode:Lwt_io.Input "test.txt"
          >>= fun channel ->
          Lwt_io.read_char channel
          >|= fun _ -> ()))

let test =
  (* Running a lot of tests in parallel allows to check for race conditions
   * see bug OF#1765
   *)
  "testRunnerProcesses" >::: (Array.to_list (Array.make 50 test))

let () =
  run_test_tt_main test
