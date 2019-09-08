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
   HTML logger for OUnit.
 *)

open OUnitLogger
open OUnitUtils
open OUnitTest
open OUnitResultSummary

let html_escaper str =
  let buffer = Buffer.create (String.length str) in
  let addc = Buffer.add_char buffer in
  let addse se =
    addc '&';
    Buffer.add_string buffer se;
    addc ';'
  in
    String.iter
      (function
         | '"' -> addse "quot"
         | '&' -> addse "amp"
         | '<' -> addse "lt"
         | '>' -> addse "gt"
(*
         | 'Œ' -> addse "OElig"
         | 'œ' -> addse "oelig"
         | 'Š' -> addse "Scaron"
         | 'š' -> addse "scaron"
         | 'Ÿ' -> addse "Yuml"
         | 'ˆ' -> addse "circ"
         | '˜' -> addse "tilde"
         | ' ' -> addse "ensp"
         | ' ' -> addse "emsp"
         | ' ' -> addse "thinsp"
         | '–' -> addse "ndash"
         | '—' -> addse "mdash"
         | '‘' -> addse "lsquo"
         | '’' -> addse "rsquo"
         | '‚' -> addse "sbquo"
         | '“' -> addse "ldquo"
         | '”' -> addse "rdquo"
         | '„' -> addse "bdquo"
         | '†' -> addse "dagger"
         | '‡' -> addse "Dagger"
         | '‰' -> addse "permil"
         | '‹' -> addse "lsaquo"
         | '›' -> addse "rsaquo"
         | '€' -> addse "euro"
 *)
         | '\'' -> addse "#39"
         | c -> addc c)
      str;
    Buffer.contents buffer

let render conf dn events =
  let smr =
    OUnitResultSummary.of_log_events conf events
  in
  let () =
    if not (Sys.file_exists dn) then
      Unix.handle_unix_error (fun () -> Unix.mkdir dn 0o755) ()
  in

  let chn = open_out (Filename.concat dn "oUnit.css") in
  let () =
    output_string chn OUnitLoggerHTMLData.oUnit_css;
    close_out chn
  in

  let chn = open_out (Filename.concat dn "oUnit.js") in
  let () =
    output_string chn OUnitLoggerHTMLData.oUnit_js;
    close_out chn
  in

  let chn = open_out (Filename.concat dn "index.html") in
  let printf fmt = Printf.fprintf chn fmt in
  printf "\
<html>
  <head>
    <title>Test suite %s</title>
    <meta http-equiv='Content-Type' content='text/html;charset=%s'/>
    <link href='oUnit.css' rel='stylesheet' type='text/css'/>
    <script language='javascript' src='oUnit.js'></script>
  </head>
  <body onload=\"displaySuccess('none');\">
    <div id='navigation'>
        <button id='toggleVisibiltySuccess'
                onclick='toggleSuccess();'>Show success</button>
        <button id='nextTest' onclick='nextTest();'>Next test</button>
        <button id='gotoTop' onclick='gotoTop();'>Goto top</button>
    </div>
    <h1>Test suite %s</h1>
    <div class='ounit-results'>
      <h2>Results</h2>
      <div class='ounit-results-content'>\n"
  (html_escaper smr.suite_name) smr.charset (html_escaper smr.suite_name);
  begin
    let printf_result clss label num =
      printf
        "<div class='ounit-results-%s'>\n\
           %s: <span class='number'>%d</span>\n\
         </div>\n"
        clss label num
    in
    let printf_non0_result clss label num =
      if num > 0 then
        printf_result clss label num
    in
      printf
        "<div id='ounit-results-started-at'>\
           Started at: %s
         </div>" (date_iso8601 smr.start_at);
      printf
        "<div class='ounit-results-duration'>\
           Total duration: <span class='number'>%.3fs</span>\
         </div>" smr.running_time;
      printf_result "test-count" "Tests count" smr.test_case_count;
      printf_non0_result "errors" "Errors" smr.errors;
      printf_non0_result "failures" "Failures" smr.failures;
      printf_non0_result "skips" "Skipped" smr.skips;
      printf_non0_result "todos" "TODO" smr.todos;
      printf_non0_result "timeouts" "Timed out" smr.timeouts;
      printf_result "successes" "Successes" smr.successes;

      (* Print final verdict *)
      if was_successful smr.global_results then
        printf "<div class='ounit-results-verdict'>Success</div>"
      else
        printf "<div class='ounit-results-verdict ounit-failure'>Failure</div>"
  end;

  printf "\
      </div>
    </div>
    <div class='ounit-conf'>
      <h2>Configuration</h2>
      <div class='ounit-conf-content'>\n";
  List.iter
    (fun (k, v) -> printf "%s=%S<br/>\n"
                     (html_escaper k) (html_escaper v))
    smr.conf;
  printf ("\
      </div>
    </div>
");
  List.iter
    (fun test_data ->
       let class_result, text_result =
         match test_data.test_result with
           | RSuccess    -> "ounit-success", "succeed"
           | RFailure _  -> "ounit-failure", "failed"
           | RError _    -> "ounit-error", "error"
           | RSkip _     -> "ounit-skip", "skipped"
           | RTodo _     -> "ounit-todo", "TODO"
           | RTimeout _  -> "ounit-timeout", "timeout"
       in
       let class_severity_opt =
         function
           | Some `Error   -> "ounit-log-error"
           | Some `Warning -> "ounit-log-warning"
           | Some `Info    -> "ounit-log-info"
           | None -> ""
       in
       printf "
    <div class='ounit-test %s'>
      <h2>%s (%s)</h2>
      <div class='ounit-started-at'>Started at: %s</div>
      <div class='ounit-duration'>Test duration: %.3fs</div>
      <div class='ounit-log'>\n"
         class_result
         (html_escaper test_data.test_name)
         (html_escaper text_result)
         (date_iso8601 test_data.timestamp_start)
         (test_data.timestamp_end -. test_data.timestamp_start);
       printf "<span class='ounit-timestamp'>%.3fs</span>Start<br/>\n"
         0.0;
       List.iter (fun (tmstp, svrt, str) ->
                    printf "\
        <span class='%s'>
          <span class='ounit-timestamp'>%.3fs</span>%s</span><br/>\n"
                      (class_severity_opt svrt) tmstp (html_escaper str))
         test_data.log_entries;
       printf "<span class='ounit-timestamp'>%.3fs</span>End<br/>\n"
         (test_data.timestamp_end -. test_data.timestamp_start);
       printf "<div class='ounit-result'>";
       begin
         (* TODO: use backtrace *)
         match test_data.test_result with
           | RSuccess -> printf "Success."
           | RFailure (str, _, _) ->
               printf "Failure:<br/>%s" (html_escaper str)
           | RError (str, _) ->
               printf "Error:<br/>%s" (html_escaper str)
           | RSkip str ->
               printf "Skipped:<br/>%s" (html_escaper str)
           | RTodo str ->
               printf "Todo:<br/>%s" (html_escaper str)
           | RTimeout test_length ->
               printf "Timeout after %.1fs<br/>"
                 (delay_of_length test_length)
       end;
       printf "</div>";
       printf "\
      </div>
    </div>\n"; (* TODO: results, end timestamp *))
    smr.tests;
  printf "\
  </body>
</html>";
  close_out chn

let output_html_dir =
  OUnitConf.make_string_subst_opt
    "output_html_dir"
    None
    "Output directory of the HTML files."

let create conf =
  match output_html_dir conf with
    | Some dn ->
        post_logger (render conf dn)
    | None ->
        null_logger
