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

(** Unit test building blocks (v2).

    @author Sylvain Le Gall
  *)

(** {2 Types} *)

(** Context of a test. *)
type test_ctxt = OUnitTest.ctxt

(** The type of test function. *)
type test_fun = test_ctxt -> unit

(** The type of test. *)
type test = OUnitTest.test

(** The expected length of the test. *)
type test_length = OUnitTest.test_length

(** {2 Assertions}

    Assertions are the basic building blocks of unittests. *)

(** Signals a failure. This will raise an exception with the specified
    string.

    @raise Failure signal a failure *)
val assert_failure : string -> 'a

(** Signals a failure when bool is false. The string identifies the
    failure.

    @raise Failure signal a failure *)
val assert_bool : string -> bool -> unit

(** Signals a failure when the string is non-empty. The string identifies the
    failure.

    @raise Failure signal a failure *)
val assert_string : string -> unit

(** [assert_command prg args] Run the command provided.

    @param exit_code expected exit code
    @param sinput provide this [char Seq.t] as input of the process
    @param foutput run this function on output, it can contains an
                   [assert_equal] to check it
    @param use_stderr redirect [stderr] to [stdout]
    @param backtrace Set OCAMLRUNPARAM=b
    @param chdir Chdir into a directory before running the command.
    @param env Unix environment
    @param verbose if a failed, dump stdout/stderr of the process to stderr
  *)
val assert_command :
    ?exit_code:Unix.process_status ->
    ?sinput:char Seq.t ->
    ?foutput:(char Seq.t -> unit) ->
    ?use_stderr:bool ->
    ?backtrace:bool ->
    ?chdir:string ->
    ?env:string array ->
    ctxt:test_ctxt ->
    string -> string list -> unit

(** [assert_equal expected real] Compares two values, when they are not equal a
    failure is signaled.

    @param cmp customize function to compare, default is [=]
    @param printer value printer, don't print value otherwise
    @param pp_diff if not equal, ask a custom display of the difference
                using [diff fmt exp real] where [fmt] is the formatter to use
    @param msg custom message to identify the failure
    @param ctxt if provided, always print expected and real value

    @raise Failure signal a failure
  *)
val assert_equal :
  ?ctxt:test_ctxt ->
  ?cmp:('a -> 'a -> bool) ->
  ?printer:('a -> string) ->
  ?pp_diff:(Format.formatter -> ('a * 'a) -> unit) ->
  ?msg:string -> 'a -> 'a -> unit

(** Asserts if the expected exception was raised.

    @param msg identify the failure

    @raise Failure description *)
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit

(** {2 Skipping tests }

    In certain condition test can be written but there is no point running it,
    because they are not significant (missing OS features for example). In this
    case this is not a failure nor a success. Following functions allow you to
    escape test, just as assertion but without the same error status.

    A test skipped is counted as success. A test todo is counted as failure.
  *)

(** [skip cond msg] If [cond] is true, skip the test for the reason explain in
    [msg]. For example [skip_if (Sys.os_type = "Win32") "Test a doesn't run on
    windows"].
  *)
val skip_if : bool -> string -> unit

(** The associated test is still to be done, for the reason given.
  *)
val todo : string -> unit

(** {2 Compare Functions} *)

(** Compare floats up to a given relative error.

    In keeping with standard floating point semantics, NaN is not equal to
    anything: [cmp_float nan nan = false].

    @param epsilon if the difference is smaller [epsilon] values are equal
  *)
val cmp_float : ?epsilon:float -> float -> float -> bool

(** {2 Bracket}

    A bracket is a registered object with setUp and tearDown in unit tests.
    Data generated during the setUp will be automatically tearDown when the test
    ends.
  *)

(** [bracket set_up tear_down test_ctxt] set up an object and register it to be
    tore down in [test_ctxt].
  *)
val bracket : (test_ctxt -> 'a) -> ('a -> test_ctxt -> unit) -> test_ctxt -> 'a

(** [bracket_tmpfile test_ctxt] Create a temporary filename and matching output
    channel. The temporary file is removed after the test.

    @param prefix see [Filename.open_temp_file]
    @param suffix see [Filename.open_temp_file]
    @param mode see [Filename.open_temp_file]
  *)
val bracket_tmpfile:
  ?prefix:string ->
  ?suffix:string ->
  ?mode:open_flag list ->
  test_ctxt -> (string * out_channel)

(** [bracket_tmpdir test_ctxt] Create a temporary dirname. The temporary
    directory is removed after the test.

    @param prefix see [Filename.open_temp_file]
    @param suffix see [Filename.open_temp_file]
  *)
val bracket_tmpdir:
  ?prefix:string ->
  ?suffix:string ->
  test_ctxt -> string

(** [with_bracket_chdir test_ctxt dn f] change directory to [dn] during
    execution of function [f]. In order to [Sys.chdir], we need to take a lock
    to avoid other tests trying to do change the current directory at the same
    time. So this bracket is not directly accessible in order to use it only on
    shorter piece of code.
  *)
val with_bracket_chdir: test_ctxt -> string -> (test_ctxt -> 'a) -> 'a

(** {2 Constructing Tests} *)

(** Create a TestLabel for a test *)
val (>:) : string -> test -> test

(** Create a TestLabel for a TestCase *)
val (>::) : string -> test_fun -> test

(** Create a TestLabel for a TestList *)
val (>:::) : string -> test list -> test

(** Generic function to create a test case. *)
val test_case : ?length:test_length -> test_fun -> test

(** Generic function to create a test list. *)
val test_list : test list -> test

(** Some shorthands which allows easy test construction.

   Examples:

   - ["test1" >: TestCase((fun _ -> ()))] =>
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test2" >:: (fun _ -> ())] =>
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test-suite" >::: ["test2" >:: (fun _ -> ());]] =>
   [TestLabel("test-suite", TestSuite([TestLabel("test2",
                                       TestCase((fun _ -> ())))]))]
*)

(** {2 Performing Tests} *)

(** Severity level for log. *)
type log_severity = [ `Error | `Warning | `Info ]

(** Log into OUnit logging system.
  *)
val logf: test_ctxt -> log_severity -> ('a, unit, string, unit) format4 -> 'a

(** Build a filename for a file that should be located in the test data dir.

    The test data dir, can be defined on the command line (preferably absolute)
    The default option is to locate it in topsrcdir/test/data.
  *)
val in_testdata_dir: test_ctxt -> string list -> string

(** [non_fatal ctxt f] Run [f] but if an exception is raised or an assert fails,
    don't stop, just register the result. The global test running result will
    mix in the non fatal result to determine the success or failure of the test.
  *)
val non_fatal: test_ctxt -> (test_ctxt -> unit) -> unit

(** Define command line options, environment variables and file configuration.

    This module helps to define configuration options that are translated to
    command line options et al.

    The name defined for the variable is:
    - should be a valid OCaml identifier
    - kept as is for use in configuration file. (foo_bar = "")
    - '_' are replaced by '-' and a leading '-' is added for command line
      (-foo "")
    - capitalized and prefixed by OUNIT_ for environment (OUNIT_FOO_BAR="")
  *)
module Conf:
sig
  (** The default type of function that create a configuration option of type
      'a.
    *)
  type 'a conf_t = string -> 'a -> Arg.doc -> test_ctxt -> 'a

  (** [make_string name default help] Create a string configuration
      option with default value [default] and a short help string.
      The result of the partial application of the function can be used
      inside tests to be evaluated to a value.

{[
let my_option = Conf.make_string "my_option" "the default" "A default option."

let tests =
  "ATest" >::
  (fun test_ctxt -> let option_value = my_option test_ctxt in ())

]}
    *)
  val make_string: string conf_t

  (** Create a [string option] configuration option. See [!make_string]. *)
  val make_string_opt: (string option) conf_t

  (** Create an [int] configuration option. See [!make_string]. *)
  val make_int: int conf_t

  (** Create a [float] configuration option. See [!make_string]. *)
  val make_float: float conf_t

  (** Create a [bool] configuration option. See [!make_string]. *)
  val make_bool: bool conf_t

  (** [make_exec execname] Create a option to define an executable. *)
  val make_exec: string -> test_ctxt -> string
end

(** Main version of the text based test runner. It reads the supplied command
    line arguments to set the verbose level and limit the number of test to
    run.

    @param test the test suite to run.
  *)
val run_test_tt_main : ?exit:(int -> unit) -> test -> unit
