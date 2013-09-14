
(*
 * Fake tests, to test runner behavior in some situation.
 *)

open OUnit2

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
  ]

let () = 
  OUnitThreads.init ();
  run_test_tt_main suite
