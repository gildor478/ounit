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

open OUnit2
open OUnitUtils

let xmllint = Conf.make_exec "xmllint"

let fakeHTML = Conf.make_exec "fakeHTML"

let tests =
  "OtherTests" >:::
  [
    "TestFakeHTML" >::
    (fun ctxt ->
       (* For easier dev. we don't use a temporary directory but a permanent
        * one, so that we can see the result.
        *)
       let () =
         skip_if (Sys.os_type = "Win32")
           "Don't run on Win32.";
         skip_if (Sys.command ((xmllint ctxt)^" --version 2> /dev/null") == 127)
           "xmllint not found.";
       in
       let html_dir = Filename.concat (Sys.getcwd ()) "log-html" in
       let junit_xml = Filename.concat html_dir "junit.xml" in
       let index_html = Filename.concat html_dir "index.html" in
       let junit_xsd = Filename.concat (Sys.getcwd ()) "JUnit.xsd" in
       let link_to_source bn =
         Sys.remove (Filename.concat html_dir bn);
         Unix.symlink
           (Filename.concat (Sys.getcwd ()) (Filename.concat "src" bn))
           (Filename.concat html_dir bn)
       in

       let grep_wc fn f =
         let count = ref 0 in
         let chn = open_in fn in
         let () =
           try
             while true do
               let line = input_line chn in
                 if f line then
                   incr count
             done;
           with End_of_file ->
             close_in chn
         in
           !count
       in

         if not (Sys.file_exists html_dir) then
           Unix.mkdir html_dir 0o750;
         assert_command
           ~ctxt
           ~exit_code:(Unix.WEXITED 1)
           (fakeHTML ctxt)
           ["-output-file"; Filename.concat html_dir "fake-html.log";
            "-output-html-dir"; html_dir;
            "-output-junit-file"; junit_xml];
         assert_equal
           ~msg:"Number of test case in junit.xml."
           ~printer:string_of_int
           6
           (grep_wc junit_xml
              (fun line -> starts_with ~prefix:"<testcase" (trim line)));
         assert_equal
           ~msg:"Number of test case in index.html."
           ~printer:string_of_int
           6
           (grep_wc index_html
              (fun line ->
                 starts_with ~prefix:"<div class='ounit-test" (trim line)));

         (* Fixing some files to use source version of it. *)
         List.iter link_to_source ["oUnit.js"; "oUnit.css"];
         assert_command
           ~ctxt
           (xmllint ctxt)
           ["--noout"; "--nonet"; "--schema"; junit_xsd; junit_xml];
         assert_command
           ~ctxt
           (xmllint ctxt)
           ["--noout"; "--nonet"; "--html"; index_html];
         (* TODO: css validation. *)
         ());

      "BacktraceProcessing" >::
      (fun _ ->

         List.iter
           (fun (str, exp) ->
              let lst = OUnitUtils.extract_backtrace_position str in
                assert_equal
                  ~printer:(fun lst ->
                              String.concat "; "
                                (List.map
                                   (function
                                      | None -> "None"
                                      | Some (f,l) ->
                                          Printf.sprintf "%S, %d" f l)
                                   lst))
                  [exp] lst)
           [
             "Raised at unknown location",
             None;

             "Raised at file \"src/oUnitAssert.ml\", line 14, characters 8-27",
             Some ("src/oUnitAssert.ml", 14);

             "Called from file \"test/testOtherTests.ml\", line 67, \
              characters 49-104",
             Some ("test/testOtherTests.ml", 67);

             "Called from file \"src/oUnitRunner.ml\", line 14, \
              characters 11-24",
             Some ("src/oUnitRunner.ml", 14);
           ]);
  ]


let test_assert_code_position =
      "AssertCodePosition" >::
      (fun ctxt ->
         skip_if (not (Printexc.backtrace_status ())) "No backtrace.";
         skip_if
          ((Printexc.get_backtrace ()) = "Called from unknown location\n")
          "No debug symbols available";
         let extract_exc e =
           let _, result, _ = OUnitTest.result_full_of_exception ctxt e in
           match result with
             | OUnitTest.RFailure (_,
                                   Some {OUnitLogger.filename = fn;
                                         line = lineno}, _) ->
                 fn, lineno
             | _ ->
                 assert_failure "Should return a position."
         in

         (* Keep the following two assert 3 lines away, *)
         let fn1, lineno1 =
           try assert_equal 1 2; "", 0 with e -> extract_exc e
         in
         let fn2, lineno2 =
           try assert_equal 2 1; "", 0 with e -> extract_exc e
         in
         let fn_exp = "test/libtest/testOtherTests.ml" in
           assert_equal ~printer:(fun s -> s) fn_exp fn1;
           assert_equal ~printer:(fun s -> s) fn_exp fn2;
           assert_equal ~printer:string_of_int 3 (lineno2 - lineno1))
