(**************************************************************************)
(* The OUnit library                                                      *)
(*                                                                        *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           *)
(* Copyright (C) 2010 OCamlCore SARL                                      *)
(* Copyright (C) 2013 Sylvain Le Gall                                     *)
(*                                                                        *)
(* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  *)
(* and Sylvain Le Gall.                                                   *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this document and the OUnit software ("the Software"), to    *)
(* deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute,           *)
(* sublicense, and/or sell copies of the Software, and to permit persons  *)
(* to whom the Software is furnished to do so, subject to the following   *)
(* conditions:                                                            *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* The Software is provided ``as is'', without warranty of any kind,      *)
(* express or implied, including but not limited to the warranties of     *)
(* merchantability, fitness for a particular purpose and noninfringement. *)
(* In no event shall Maas-Maarten Zeeman be liable for any claim, damages *)
(* or other liability, whether in an action of contract, tort or          *)
(* otherwise, arising from, out of or in connection with the Software or  *)
(* the use or other dealings in the software.                             *)
(*                                                                        *)
(* See LICENSE.txt for details.                                           *)
(**************************************************************************)

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
      ?exit_code ?sinput ?foutput ?use_stderr ?backtrace ?chdir ?env ~ctxt
      prg args =
  OUnitAssert.assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?backtrace ?chdir ?env ~ctxt
      prg args
let assert_equal = OUnitAssert.assert_equal
let assert_raises = OUnitAssert.assert_raises
let skip_if = OUnitAssert.skip_if
let todo = OUnitAssert.todo
let cmp_float = OUnitUtils.cmp_float
let bracket = OUnitBracket.create
let bracket_tmpfile = OUnitBracket.bracket_tmpfile
let bracket_tmpdir = OUnitBracket.bracket_tmpdir
let with_bracket_chdir test_ctxt dn f =
  OUnitBracket.with_bracket test_ctxt
    (OUnitBracket.bracket_chdir dn)
    (fun _ -> f)


let non_fatal = OUnitTest.non_fatal
let run_test_tt_main = OUnitCore.run_test_tt_main

let logf ctxt log_severity fmt =
   OUnitLogger.Test.logf ctxt.test_logger log_severity fmt

let in_testdata_dir ctxt path =
  OUnitTestData.in_testdata_dir ctxt.conf path

let conf_wrap f name default help =
  let get = f name default help in
    fun ctxt -> get ctxt.conf

module Conf =
struct
  type 'a conf_t = string -> 'a -> Arg.doc -> test_ctxt -> 'a
  let make_string = conf_wrap OUnitConf.make_string
  let make_string_opt = conf_wrap OUnitConf.make_string_opt
  let make_int = conf_wrap OUnitConf.make_int
  let make_float = conf_wrap OUnitConf.make_float
  let make_bool = conf_wrap OUnitConf.make_bool
  let make_exec name =
    let get = OUnitConf.make_exec name in
      fun ctxt -> get ctxt.conf
end
