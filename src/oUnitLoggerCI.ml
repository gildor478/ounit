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
   CI logger for OUnit (Travis, AppVeyor...).

   This logger allows to print results and logs in CI tools like Travis and
   AppVeyor.
 *)

open OUnitLogger
open OUnitResultSummary
open OUnitTest

let printlf color fmt =
  let ansi_color =
    match color with
    | `Red -> "31"
    | `Green -> "32"
    | `Yellow -> "33"
    | `None -> ""
  in
  Printf.fprintf stdout "\027[%sm" ansi_color;
  Printf.kfprintf (fun chn -> Printf.fprintf chn "\027[0m\n") stdout fmt

let successes_color = `None
let errors_color = `Red
let failures_color = `Red
let skips_color = `Yellow
let todos_color = `Yellow
let timeouts_color = `Red

let severity =
 function
 | Some `Error   -> `Red, "E"
 | Some `Warning -> `Yellow, "W"
 | Some `Info    -> `None, "I"
 | None -> `None, "I"

let print_separator () = printlf `None "========================="

let render conf events =
  let smr = OUnitResultSummary.of_log_events conf events in
  List.iter
    (fun test_data ->
       print_separator ();
       printlf `None "%s" test_data.test_name;
       begin
         match test_data.test_result with
           | RSuccess ->
             printlf successes_color "Success"
           | RFailure (str, _, backtrace) ->
             printlf failures_color "Failure: %s" str;
             begin
               match backtrace with
               | Some txt -> printlf failures_color "Backtrace: %s" txt
               | None -> ()
             end
           | RError (str, backtrace) ->
             printlf errors_color "Error: %s" str;
             begin
               match backtrace with
               | Some txt -> printlf errors_color "Backtrace: %s" txt
               | None -> ()
             end
           | RSkip str ->
             printlf skips_color "Skipped: %s" str;
           | RTodo str ->
             printlf todos_color "TODO: %s" str;
           | RTimeout test_length ->
             printlf timeouts_color
               "Timeout %.1fs" (delay_of_length test_length)
       end;
       printlf `None "Logs:";
       List.iter
         (fun (tmstp, svrt, str) ->
            let color, prefix = severity svrt in
            printlf color "%04.1fs %s: %s" tmstp prefix str)
         test_data.log_entries;
       if List.length test_data.log_entries <> 0 then
         printlf `None "%04.1fs I: End"
           (test_data.timestamp_end -. test_data.timestamp_start);
    )
    (List.filter
       (fun test_data -> test_data.test_result <> RSuccess) smr.tests);
  print_separator ();
  printlf `None   "Summary:";
  printlf `None   "Tried tests:     %d" smr.test_case_count;
  printlf `Red    "Errors:          %d" smr.errors;
  printlf `Red    "Failures:        %d" smr.failures;
  printlf `Yellow "Skipped tests:   %d" smr.skips;
  printlf `Yellow "TODO tests:      %d" smr.todos;
  printlf `Red    "Timed-out tests: %d" smr.timeouts;
  ()

let ci =
  OUnitConf.make_bool
    "ci"
    false
    "Display logs for CI, like Travis and AppVeyor, in the console with colors."

let create conf =
  if ci conf then
    post_logger (render conf)
  else
    null_logger
