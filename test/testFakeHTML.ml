

(*
 * Fake tests, to test HTML output.
 *)

open OUnit2

let suite =
  "OUnitLoggerHTML" >:::
  [
    "first test" >::
    (fun ctxt ->
       assert_equal 0 1);

    "second test" >::
    (fun ctxt ->
       assert_equal 0 0);

    "third test" >::
    (fun ctxt ->
       skip_if true "skipped because of me");

    "fourth test" >::
    (fun ctxt ->
       todo "need to make this function");

    "fifth test" >::
    (fun ctxt ->
       raise Not_found);

    "with symbol" >::
    (fun ctxt ->
       failwith "this is a bad message: '\"&<>")
  ]

let () = run_test_tt_main suite
