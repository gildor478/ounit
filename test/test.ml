
open OUnit2

let rec ounit2_of_ounit1= 
  function
    | OUnit.TestCase f ->
        OUnit2.TestCase (fun ctxt -> f ())
    | OUnit.TestList lst ->
        OUnit2.TestList (List.map ounit2_of_ounit1 lst)
    | OUnit.TestLabel (lbl, test) ->
        OUnit2.TestLabel (lbl, ounit2_of_ounit1 test)

let () =
  run_test_tt_main
    ("OUnit" >:::
     [
       ounit2_of_ounit1 TestOUnit1.tests;
       TestOUnit2.tests;
       TestConf.tests;
       TestOtherTests.tests;
     ])

