
open OUnit2

let () =
  run_test_tt_main
    ("OUnit" >:::
     [
       OUnit.ounit2_of_ounit1 TestOUnit1.tests;
       TestOUnit2.tests;
       TestConf.tests;
       TestOUnitTest.tests;
       TestOUnitAssert.tests;
       TestOUnitDiff.tests;
       TestOtherTests.tests;
       TestRunner.tests;
     ])

