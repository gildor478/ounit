
(*
 * Fake tests, to test runner behavior in some situation.
 *)

open OUnit2

let sigsegv =
  conf_make_bool
    "sigsegv"
    false
    "Fail with SIGSEGV."

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
         Unix.kill (Unix.getpid ()) 11)
  ]

let () = 
  OUnitThreads.init ();
  run_test_tt_main suite
