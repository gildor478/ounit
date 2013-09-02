include OUnitTypes
open OUnitCore

let (>:) = OUnitTest.(>:)
let (>::) = OUnitTest.(>::)
let (>:::) = OUnitTest.(>:::)
let (@?) = OUnitAssert.assert_bool

let assert_failure = OUnitAssert.assert_failure
let assert_bool = OUnitAssert.assert_bool
let assert_string = OUnitAssert.assert_string
(* Upgrade to OUnit v2, using logger. *)
(* let assert_command = OUnitAssert.assert_command *)
let assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ~ctxt prg args =
  OUnitAssert.assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ~ctxt
      prg args
let assert_equal = OUnitAssert.assert_equal
let assert_raises = OUnitAssert.assert_raises
let skip_if = OUnitAssert.skip_if
let todo = OUnitAssert.todo
let cmp_float = OUnitUtils.cmp_float
let bracket = OUnitBracket.bracket
let bracket_tmpfile = OUnitBracket.bracket_tmpfile
let bracket_tmpdir = OUnitBracket.bracket_tmpdir
(* TODO: remove, advanced use. *)
let test_decorate = OUnitTest.test_decorate
(* TODO: remove, advanced use. *)
let test_filter = OUnitTest.test_filter
(* TODO: remove, advanced use. *)
let test_case_count = OUnitTest.test_case_count
(* TODO: remove, advanced use. *)
let string_of_node = OUnitTest.string_of_node
(* TODO: remove, advanced use. *)
let string_of_path = OUnitTest.string_of_path
(* TODO: remove, advanced use. *)
let test_case_paths = OUnitTest.test_case_paths
(* TODO: remove, advanced use. *)
let perform_test =
  OUnitCore.perform_test
    (OUnitConf.default ())
    OUnitRunner.default
    OUnitChooser.default
(* TODO: remove, advanced use. *)
let run_test_tt ?verbose tests =
  OUnitCore.run_test_tt
    (OUnitConf.default ())
    OUnitRunner.default
    OUnitChooser.default
    tests
let run_test_tt_main = OUnitCore.run_test_tt_main
(* TODO move this to OUnitCore. *)
let logf ctxt log_severity fmt =
   OUnitLogger.Test.logf ctxt.logger log_severity fmt
let conf_make = OUnitCore.conf_make
