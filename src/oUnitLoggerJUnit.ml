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

(*
   JUnit logger for OUnit.
 *)

open OUnitLogger
open OUnitUtils
open OUnitTest
open OUnitResultSummary


let xml_escaper = OUnitLoggerHTML.html_escaper

let render conf fn events =
  let smr =
    OUnitResultSummary.of_log_events conf events
  in
  let chn = open_out fn in
  let string_of_failure =
    function
      | msg, None ->
          msg^"\nNo backtrace."
      | msg, Some backtrace ->
          msg^"\n"^backtrace
  in
  let printf fmt = Printf.fprintf chn fmt in
    printf "\
<?xml version='1.0' encoding='%s'?>
<testsuites>
  <testsuite
      id='0'
      package='%s'
      name='%s'
      timestamp='%s'
      hostname='%s'
      tests='%d'
      failures='%d'
      errors='%d'
      time='%f'>\n"
    smr.charset
    (xml_escaper smr.suite_name)
    (xml_escaper smr.suite_name)
    (xml_escaper (date_iso8601 ~tz:false smr.start_at))
    (xml_escaper (fqdn ()))
    smr.test_case_count
    (smr.failures + smr.todos)
    smr.errors
    smr.running_time;
    printf "\
\    <properties>\n";
    List.iter
      (fun (k, v) ->
         printf "\
\      <property name='%s' value='%s' />\n"
           (xml_escaper k) (xml_escaper v))
      smr.conf;
    printf "\
\    </properties>\n";
    List.iter
      (fun test_data ->
         printf "\
\    <testcase name='%s' classname='%s' time='%f'>\n"
           (xml_escaper test_data.test_name)
           (xml_escaper test_data.test_name)
           (test_data.timestamp_end -. test_data.timestamp_start);
         begin
           match test_data.test_result with
             | RSuccess | RSkip _ ->
                 ()
             | RError (msg, backtrace) ->
                 printf "\
\      <error type='OUnit.Error' message='%s'>%s</error>\n"
                   (xml_escaper msg)
                   (xml_escaper (string_of_failure (msg, backtrace)))
             | RFailure (msg, _, backtrace) ->
                 printf "\
\      <failure type='OUnit.Failure' message='%s'>%s</failure>\n"
                   (xml_escaper msg)
                   (xml_escaper (string_of_failure (msg, backtrace)))
             | RTodo msg ->
                 printf "\
\      <failure type='OUnit.Todo' message='%s'></failure>\n"
                   (xml_escaper msg)
             | RTimeout test_length ->
                 printf "\
\      <error type='OUnit.Timeout' message='timeout after %.1fs'></error>\n"
                   (delay_of_length test_length)
         end;
         printf "\
\    </testcase>\n")
      smr.tests;
    printf "\
\    <system-out>\n";
    List.iter
      (fun log_event ->
         List.iter (fun s -> printf "%s\n" (xml_escaper s))
           (OUnitLoggerStd.format_log_event log_event))
      events;
    printf "\
\    </system-out>
    <system-err />
  </testsuite>
</testsuites>
";
    close_out chn

let output_junit_file =
  OUnitConf.make_string_subst_opt
    "output_junit_file"
    None
    "Output file for JUnit."

let create conf =
  match output_junit_file conf with
    | Some fn ->
        post_logger (render conf fn)
    | None ->
        null_logger
