

(*
 * Fake tests, to test HTML output.
 *)

open OUnit

let suite = 
  "OUnitLoggerHTML" >:::
  [
    "first test" >::
    (fun () ->
       assert_equal 0 1);

    "second test" >::
    (fun () ->
       assert_equal 0 0);

    "third test" >::
    (fun () ->
       skip_if true "skipped because of me");

    "fourth test" >::
    (fun () ->
       todo "need to make this function");

    "fifth test" >::
    (fun () ->
       raise Not_found);
  ]

let _ =
  run_test_tt_main suite
