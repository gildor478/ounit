
(*
 * Fake tests, to test mutex behavior with different runner.
 *)

open OUnit2
open OUnitShared

let test_mutex ctxt mutex =
  let shared = ctxt.OUnitTest.shared in
    Mutex.lock shared mutex;
    assert_bool
      "Cannot acquire a locked mutex."
      (not (Mutex.try_lock shared mutex));
    Mutex.unlock shared mutex;
    assert_bool
      "Can acquire an unlocked mutex."
      (Mutex.try_lock shared mutex);
    Mutex.unlock shared mutex

let tests =
  "Shared" >:::
  [
    "MutexGlobal" >::
    (fun ctxt ->
       test_mutex ctxt (Mutex.create ScopeGlobal));

    "MutexProcess" >::
    (fun ctxt ->
       test_mutex ctxt (Mutex.create ScopeProcess));
  ]

let () =
  OUnitThreads.init ();
  run_test_tt_main tests
