(*
   Summary of the results, based on captured log events.
 *)

open OUnitUtils
open OUnitTest
open OUnitLogger

type log_entry =
    float (* time since start of the test *) *
    log_severity option *
    string (* log entry without \n *)

type test_data =
    {
      test_name: string;
      timestamp_start: float;      (* UNIX timestamp *)
      timestamp_end: float;        (* UNIX timestamp *)
      log_entries: log_entry list; (* time sorted log entry, timestamp from
                                      timestamp_start *)
      test_result: OUnitTest.result;
    }

type t =
    {
      suite_name: string;
      start_at: float;
      charset: string;
      conf: (string * string) list;
      running_time: float;
      global_results: OUnitTest.result_list;
      test_case_count: int;
      tests: test_data list;
      errors: int;
      failures: int;
      skips: int;
      todos: int;
      successes: int;
    }

let is_success =
  function
    | RSuccess -> true
    | RFailure _ | RError _  | RSkip _ | RTodo _ -> false

let is_failure =
  function
    | RFailure _ -> true
    | RSuccess | RError _  | RSkip _ | RTodo _ -> false

let is_error =
  function
    | RError _ -> true
    | RSuccess | RFailure _ | RSkip _ | RTodo _ -> false

let is_skip =
  function
    | RSkip _ -> true
    | RSuccess | RFailure _ | RError _  | RTodo _ -> false

let is_todo =
  function
    | RTodo _ -> true
    | RSuccess | RFailure _ | RError _  | RSkip _ -> false

let result_flavour =
  function
    | RError _ -> "Error"
    | RFailure _ -> "Failure"
    | RSuccess -> "Success"
    | RSkip _ -> "Skip"
    | RTodo _ -> "Todo"

let result_msg =
  function
    | RSuccess -> "Success"
    | RError (msg, _)
    | RFailure (msg, _)
    | RSkip msg
    | RTodo msg -> msg

let was_successful lst =
  List.for_all
    (fun (_, rslt, _) ->
       match rslt with
         | RSuccess | RSkip _ -> true
         | _ -> false)
    lst

let encoding =
  OUnitConf.make_string
    "log_encoding"
    "utf-8"
    "Encoding of the log."

let of_log_events conf events =
  let global_conf =
    List.fold_left
      (fun acc log_ev ->
         match log_ev.event with
           | GlobalEvent (GConf (k, v)) -> (k, v) :: acc
           | _ -> acc)
      []
      (List.rev events)
  in
  let running_time, global_results, test_case_count =
    let rec find_results =
      function
        | {event =
             GlobalEvent
               (GResults (running_time, results, test_case_count))} :: _ ->
            running_time, results, test_case_count
        | _ :: tl ->
            find_results tl
        | [] ->
            failwith "Cannot find results in OUnitResult.of_log_events."
    in
      find_results events
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

    let finalize t =
      let log_entries =
        List.sort
          (fun (f1, _, _) (f2, _, _) -> Pervasives.compare f2 f1)
          t.log_entries
      in
      let log_entries =
        List.rev_map
          (fun (f, a, b) -> f -. t.timestamp_start, a, b)
          log_entries
      in
        {t with log_entries = log_entries}
    in

    let default_timestamp = 0.0 in
    let rec process_log_event tests log_event =
      let timestamp = log_event.timestamp in
        match log_event.event with
          | GlobalEvent _ ->
              tests
          | TestEvent (path, ev)  ->
              begin
                let t =
                  try
                    MapPath.find path tests
                  with Not_found ->
                    {
                      test_name = string_of_path path;
                      timestamp_start = default_timestamp;
                      timestamp_end = default_timestamp;
                      log_entries = [];
                      test_result = RFailure ("Not finished", None);
                    }
                in
                let alt0 t1 t2 =
                  if t1 = default_timestamp then
                    t2
                  else
                    t1
                in
                let t' =
                  match ev with
                    | EStart ->
                        {t with
                             timestamp_start = timestamp;
                             timestamp_end = alt0 t.timestamp_end timestamp}
                    | EEnd ->
                        {t with
                             timestamp_end = timestamp;
                             timestamp_start = alt0 t.timestamp_start timestamp}
                    | EResult rslt ->
                        {t with test_result = rslt}
                    | ELog (svrt, str) ->
                        {t with log_entries = (timestamp, Some svrt, str)
                         :: t.log_entries}
                    | ELogRaw str ->
                        {t with log_entries =
                           split_raw timestamp str t.log_entries}
                in
                  MapPath.add path t' tests
              end
    and group_test tests =
      function
        | hd :: tl ->
            group_test
              (process_log_event tests hd)
              tl
        | [] ->
            let lst =
              MapPath.fold
                (fun _ test lst ->
                   finalize test :: lst)
                tests []
            in
              List.sort
                (fun t1 t2 ->
                   Pervasives.compare t1.timestamp_start t2.timestamp_start)
                lst
    in
      group_test MapPath.empty events
  in
  let start_at =
    List.fold_left
      (fun start_at log_ev ->
         min start_at log_ev.timestamp)
      (now ())
      events
  in
  let suite_name =
    match global_results with
      | (path, _, _) :: _ ->
          List.fold_left
            (fun acc nd ->
               match nd with
                 | ListItem _ -> acc
                 | Label str -> str)
            "noname"
            path
      | [] ->
          "noname"
  in
  let count f =
    List.length
      (List.filter (fun (_, test_result, _) -> f test_result)
         global_results)
  in
  let errors   = count is_error in
  let failures = count is_failure in
  let skips    = count is_skip in
  let todos    = count is_todo in
  let successes = count is_success in
  let charset = encoding conf in
    {
      suite_name      = suite_name;
      start_at        = start_at;
      charset         = charset;
      conf            = global_conf;
      running_time    = running_time;
      global_results  = global_results;
      test_case_count = test_case_count;
      tests           = tests;
      errors          = errors;
      failures        = failures;
      skips           = skips;
      todos           = todos;
      successes       = successes;
    }
