open OUnitCore
open OUnitTest

let (>:) = OUnitTest.(>:)
let (>::) = OUnitTest.(>::)
let (>:::) = OUnitTest.(>:::)

type test_ctxt = OUnitTest.ctxt
type test_fun = OUnitTest.test_fun
type test_length = OUnitTest.test_length
type test = OUnitTest.test
let test_case ?(length=Short) f = TestCase(length, f)
let test_list lst = TestList lst

type log_severity = OUnitLogger.log_severity

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
let bracket = OUnitBracket.create
let bracket_tmpfile = OUnitBracket.bracket_tmpfile
let bracket_tmpdir = OUnitBracket.bracket_tmpdir
let non_fatal = OUnitTest.non_fatal
let run_test_tt_main = OUnitCore.run_test_tt_main

let logf ctxt log_severity fmt =
   OUnitLogger.Test.logf ctxt.test_logger log_severity fmt

let conf_wrap f name default help =
  let get = f name default help in
    fun ctxt -> get ctxt.conf

let conf_make_string = conf_wrap OUnitConf.make_string
let conf_make_string_opt = conf_wrap OUnitConf.make_string_opt
let conf_make_int = conf_wrap OUnitConf.make_int
let conf_make_float = conf_wrap OUnitConf.make_float
let conf_make_bool = conf_wrap OUnitConf.make_bool
