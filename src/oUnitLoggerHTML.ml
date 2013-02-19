(*
   HTML logger for OUnit.
 *)

open OUnitTypes
open OUnitLogger
open OUnitUtils

type log_entry = 
    float (* time since start of the test *) * 
    log_severity option *
    string (* log entry without \n *)
type test_data =
    {
      test_name: string;
      timestamp_start: float; (* UNIX timestamp *)
      timestamp_end: float; (* UNIX timestamp *)
      log_entries: log_entry list; (* time sorted log entry *)
      test_result: test_result;
    }

let global_output_html_dir = 
  let value =
    OUnitConf.make
      "output_html_dir"
      (fun r -> Arg.Set_string r)
      ~printer:(Printf.sprintf "%S")
      ""
      "Output directory of the HTML files."
  in
    fun () ->
      match value () with 
        | "" -> None
        | fn -> Some fn

let render dn rev_events = 
  let () =
    if not (Sys.file_exists dn) then
      Unix.handle_unix_error (fun () -> Unix.mkdir dn 0o755) ()
  in
  let conf = 
    List.fold_left
      (fun acc ev ->
         match ev with 
           | GlobalEvent (GConf str) -> str :: acc
           | _ -> acc)
      []
      rev_events
  in
  let running_time, global_results, test_case_count =
    let rec find_results =
      function
        | GlobalEvent (GResults (running_time, results, test_case_count)) :: _ ->
            running_time, results, test_case_count
        | _ :: tl ->
            find_results tl
        | [] ->
            failwith "Cannot find results in OUnitLoggerHTML"
    in
      find_results rev_events
  in
  let tests = 
    let rec split_raw tmstp str lst =
      try
        let idx = String.index str '\n' in
          split_raw tmstp 
            (String.sub str (idx + 1) (String.length str - idx - 1))
            ((tmstp, None, String.sub str 0 idx) :: lst)
      with Not_found ->
        (tmstp, None, str) :: lst
    in

    let finalize t tests =
      {t with log_entries = List.rev t.log_entries} :: tests
    in
      
    let rec parse_test tests t = 
      function 
        | GlobalEvent _ :: tl ->
            parse_test tests t tl
        | TestEvent ev :: tl  ->
            begin 
              match ev with 
                | EStart _ ->
                    group_test
                      (t :: tests)
                      (TestEvent ev :: tl)
                | EEnd _ ->
                    group_test
                      (finalize t tests)
                      tl
                | EResult rslt ->
                    parse_test
                      tests
                      {t with test_result = rslt}
                      tl
                | ELog (svrt, str) ->
                    parse_test
                      tests
                      {t with log_entries = (0.0, Some svrt, str) :: t.log_entries}
                      tl
                | ELogRaw str ->
                    parse_test 
                      tests
                      {t with log_entries = split_raw 0.0 str t.log_entries}
                      tl
            end
        | [] ->
            group_test (finalize t tests) []

    and group_test tests = 
      function 
        | GlobalEvent _ :: tl ->
            group_test tests tl
        | TestEvent ev :: tl->
            begin
              match ev with 
                | EStart path ->
                    parse_test
                      tests
                      {
                        test_name = string_of_path path;
                        timestamp_start = 0.0;
                        timestamp_end = 0.0;
                        log_entries = [];
                        test_result = RFailure (path, "Not finished");
                      }
                      tl
                | _ ->
                    failwith 
                      (Printf.sprintf
                         "Expected EStart _ got %s"
                         (string_of_event (TestEvent ev)))
            end
        | [] ->
            List.rev tests
    in
      group_test [] (List.rev rev_events)
  in
  let suite_name = "OUnit" in
  let charset = "utf-8" in

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
        <button id='toggleVisibiltySuccess' onclick='toggleSuccess();'>Show success</button>
        <button id='nextTest' onclick='nextTest();'>Next test</button>
        <button id='gotoTop' onclick='gotoTop();'>Goto top</button>
    </div>
    <div class='ounit-results'>
      <h1>Results</h1>
      <div class='ounit-results-content'>\n"
  suite_name charset; 
  begin
    let count f = 
      List.length 
        (List.filter (fun (test_result, _) -> f test_result)
           global_results)
    in
    let errors   = count is_error in
    let failures = count is_failure in
    let skips    = count is_skip in
    let todos    = count is_todo in
    let successes = count is_success in
    let printf_result clss label num =
      printf 
        "<div class='ounit-results-%s'>\
           %s: <span class='number'>%d</span>\
         </div>"
        clss label num
    in
    let printf_non0_result clss label num =
      if num > 0 then
        printf_result clss label num
    in
      printf 
        "<div class='ounit-results-duration'>\
           Total duration: <span class='number'>%.3fs</span>\
         </div>" running_time;
      printf_result "test-count" "Tests count" test_case_count;
      printf_non0_result "errors" "Errors" errors;
      printf_non0_result "failures" "Failures" failures;
      printf_non0_result "skips" "Skipped" skips;
      printf_non0_result "todos" "TODO" todos;
      printf_result "successes" "Successes" successes;

      (* Print final verdict *)
      if was_successful (List.rev_map fst global_results) then 
        printf "<div class='ounit-results-verdict'>Success</div>"
      else
        printf "<div class='ounit-results-verdict ounit-failure'>Failure</div>"
  end;

  printf "\
      </div>
    </div>
    <div class='ounit-conf'>
      <h1>Configuration</h1>
      <div class='ounit-conf-content'>\n";
  List.iter (printf "%s<br/>\n") conf;
  printf ("\
      </div>
    </div>
");
  List.iter
    (fun test_data ->
       let class_result, text_result = 
         match test_data.test_result with 
           | RSuccess _      -> "ounit-success", "succeed"
           | RFailure (_, _) -> "ounit-failure", "failed"
           | RError (_, _)   -> "ounit-error", "error"
           | RSkip (_, _)    -> "ounit-skip", "skipped"
           | RTodo (_, _)    -> "ounit-todo", "TODO"
       in
       let class_severity_opt = 
         function
           | Some LError   -> "ounit-log-error"
           | Some LWarning -> "ounit-log-warning"
           | Some LInfo    -> "ounit-log-info"
           | None -> ""
       in
       printf "
    <div class='ounit-test %s'>
      <h1>%s (%s)</h1>
      <div class='ounit-duration'>Test duration: %0.3fs</div>
      <div class='ounit-log'>\n" 
         class_result
         test_data.test_name 
         text_result
         (test_data.timestamp_end -. test_data.timestamp_start);
       printf "<span class='ounit-timestamp'>%0.3fs</span>Start<br/>\n" 
         test_data.timestamp_start;
       List.iter (fun (tmstp, svrt, str) ->
                    printf "\
        <span class='%s'><span class='ounit-timestamp'>%0.3fs</span>%s</span><br/>\n" 
                      (class_severity_opt svrt) tmstp str)
         test_data.log_entries;
       printf "<span class='ounit-timestamp'>%0.3fs</span>End<br/>\n" 
         test_data.timestamp_start;
       printf "<div class='ounit-result'>";
       begin
         match test_data.test_result with 
           | RSuccess _ -> printf "Success."
           | RFailure (_, str) -> printf "Failure:<br/>%s" str
           | RError (_, str) -> printf "Error:<br/>%s" str
           | RSkip (_, str) -> printf "Skipped:<br/>%s" str
           | RTodo (_, str) -> printf "Todo:<br/>%s" str
       end;
       printf "</div>";
       printf "\
      </div>
    </div>\n"; (* TODO: results, end timestamp *))
    tests;
  printf "\
  </body>
</html>";
  close_out chn

let create () =
  match global_output_html_dir () with 
    | Some dn ->
        let data = ref [] in
        let fwrite ev = data := ev :: !data in
        let fclose () = render dn !data in
          {
            fwrite = fwrite;
            fpos   = (fun () -> None);
            fclose = fclose;
          }
    | None ->
        null_logger
