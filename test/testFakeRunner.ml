
(*
 * Fake tests, to test runner behavior in some situation.
 *)

open OUnit2

let sigsegv =
  Conf.make_bool
    "sigsegv"
    false
    "Fail with SIGSEGV."

let timeout =
  Conf.make_bool
    "timeout"
    false
    "Time out."

let suite =
  "TestFakeRunner" >:::
  [
    "success" >::
    (fun ctxt -> assert_equal 0 0);

    "failure" >::
    (fun ctxt -> assert_equal 0 1);

    "todo" >::
    (fun ctxt -> skip_if true "skipped because of me");

    "skip" >::
    (fun ctxt -> todo "need to make this function");

    "error" >::
    (fun ctxt -> raise Not_found);

    "SIGSEGV" >::
    (fun ctxt ->
       if sigsegv ctxt then
         Unix.kill (Unix.getpid ()) 11);

    "Timeout" >:
    (test_case
       ~length:(OUnitTest.Custom_length 0.1)
       (fun ctxt ->
          if timeout ctxt then
            Unix.sleep 1))
  ]

let () = 
  OUnitThreads.init ();
  run_test_tt_main suite
