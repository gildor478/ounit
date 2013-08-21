(*
   JUnit logger for OUnit.
 *)

open OUnitTypes
open OUnitLogger
open OUnitUtils
open OUnitResultSummary


let global_output_junit_file =
  let value =
    OUnitConf.make
      "output_junit_file"
      (fun r -> Arg.Set_string r)
      ~printer:(Printf.sprintf "%S")
      ""
      "Output file for JUnit."
  in
    fun () ->
      match value () with
        | "" -> None
        | fn -> Some fn

let xml_escaper = OUnitLoggerHTML.html_escaper

let render fn events =
  let smr =
    OUnitResultSummary.of_log_events events
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
    (xml_escaper (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name)
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
             | RFailure (msg, backtrace) ->
                 printf "\
\      <failure type='OUnit.Failure' message='%s'>%s</failure>\n"
                   (xml_escaper msg)
                   (xml_escaper (string_of_failure (msg, backtrace)))
             | RTodo msg ->
                 printf "\
\      <failure type='OUnit.Failure' message='%s'></failure>\n"
                   (xml_escaper msg)
         end;
         printf "\
\    </testcase>\n")
      smr.tests;
    printf "\
\    <system-out>\n";
    List.iter
      (fun log_event ->
         printf "%s" (xml_escaper (OUnitLogger.format_event true log_event)))
      events;
    printf "\
\    </system-out>
    <system-err />
  </testsuite>
</testsuites>
";
    close_out chn


let create () =
  match global_output_junit_file () with
    | Some fn ->
        post_logger (render fn)
    | None ->
        null_logger
