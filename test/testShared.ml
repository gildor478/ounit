
open OUnit2

let testFakeShared =
  Conf.make_string
    "testFakeShared"
    "testFakeShared"
    "The testFakeShared executable to run."

let run_test_fake_shared ctxt runner args =
  let fn, _ = bracket_tmpfile ctxt in
    assert_command
      ~ctxt
      ~exit_code:(Unix.WEXITED 0)
      (testFakeShared ctxt)
      ("-output-file" :: fn :: "-runner" :: runner :: args)

let tests =
  "Shared" >:::
  [
    "Sequential" >::
    (fun ctxt ->
       run_test_fake_shared ctxt "sequential" []);

    "Processes" >::
    (fun ctxt ->
       run_test_fake_shared ctxt "processes" ["-shards"; "2"]);

    "Threads" >::
    (fun ctxt ->
       run_test_fake_shared ctxt "threads" ["-shards"; "2"]);
  ]

