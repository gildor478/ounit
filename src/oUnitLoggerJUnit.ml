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

let render fn events = 
  let smr = 
    OUnitResultSummary.of_log_events events
  in
  let chn = open_out fn in
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
    smr.suite_name 
    smr.suite_name
    (date_iso8601 ~tz:false smr.start_at)
    (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name
    smr.test_case_count 
    (smr.failures + smr.todos)
    smr.errors
    smr.running_time;
    (* TODO: properties. *)
    printf "<properties />";
    List.iter
      (fun test_data ->
         printf "\
\    <testcase name='%s' classname='%s' time='%f'>\n"
           test_data.test_name
           test_data.test_name (* TODO: use basename and full path of the test. *)
           (test_data.timestamp_end -. test_data.timestamp_start);
         begin
           match test_data.test_result with 
             | RSuccess | RSkip _ ->
                 ()
             | RError (msg, backtrace) ->
                 printf "\
\      <error type='OUnit.Error' message='%s'></error>\n" msg
                   (* TODO: content. *)
             | RFailure (msg, backtrace) ->
                 printf "\
\      <failure type='OUnit.Failure' message='%s'></failure>\n" msg
                   (* TODO: content. *)
             | RTodo msg ->
                 printf "\
\      <failure type='OUnit.Failure' message='%s'></failure>\n" msg
         end;
         printf "\
\    </testcase>\n")
      smr.tests;
    printf "\
\    <system-out />
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
