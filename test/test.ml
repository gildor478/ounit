
open OUnit2

let rec ounit2_of_ounit1 =
  function
    | OUnit.TestCase f ->
        test_case (fun ctxt -> f ())
    | OUnit.TestList lst ->
        test_list (List.map ounit2_of_ounit1 lst)
    | OUnit.TestLabel (lbl, test) ->
        lbl >: (ounit2_of_ounit1 test)

let () =
  run_test_tt_main
    ("OUnit" >:::
     [
       ounit2_of_ounit1 TestOUnit1.tests;
       TestOUnit2.tests;
       TestConf.tests;
       TestOUnitTest.tests;
       TestOUnitAssert.tests;
       TestOUnitDiff.tests;
       TestOtherTests.tests;
     ])

