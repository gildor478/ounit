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
      timeouts: int;
      successes: int;
    }

let is_success =
  function
    | RSuccess -> true
    | RFailure _ | RError _  | RSkip _ | RTodo _ | RTimeout _ -> false

let is_failure =
  function
    | RFailure _ -> true
    | RSuccess | RError _  | RSkip _ | RTodo _ | RTimeout _ -> false

let is_error =
  function
    | RError _ -> true
    | RSuccess | RFailure _ | RSkip _ | RTodo _ | RTimeout _ -> false

let is_skip =
  function
    | RSkip _ -> true
    | RSuccess | RFailure _ | RError _  | RTodo _ | RTimeout _ -> false

let is_todo =
  function
    | RTodo _ -> true
    | RSuccess | RFailure _ | RError _  | RSkip _ | RTimeout _ -> false

let is_timeout =
  function
    | RTimeout _ -> true
    | RSuccess | RFailure _ | RError _  | RSkip _ | RTodo _ -> false

let result_flavour =
  function
    | RError _ -> "Error"
    | RFailure _ -> "Failure"
    | RSuccess -> "Success"
    | RSkip _ -> "Skip"
    | RTodo _ -> "Todo"
    | RTimeout _ -> "Timeout"

let result_msg =
  function
    | RSuccess -> "Success"
    | RError (msg, _)
    | RFailure (msg, _, _)
    | RSkip msg
    | RTodo msg -> msg
    | RTimeout test_length ->
        Printf.sprintf "Timeout after %.1fs" (delay_of_length test_length)

let worst_cmp result1 result2 =
  let rank =
    function
      | RSuccess -> 0
      | RSkip _ -> 1
      | RTodo _ -> 2
      | RFailure _ -> 3
      | RError _ -> 4
      | RTimeout _ -> 5
  in
    (rank result1) - (rank result2)

let worst_result_full result_full lst =
  let worst =
    List.fold_left
      (fun ((_, result1, _) as result_full1)
             ((_, result2, _) as result_full2) ->
       if worst_cmp result1 result2 < 0 then
           result_full2
         else
           result_full1)
      result_full lst
  in
    worst,
    List.filter
      (fun result_full -> not (result_full == worst))
      (result_full :: lst)

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
               (GResults (running_time, results, test_case_count)); _} :: _ ->
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
          (fun (f1, _, _) (f2, _, _) -> Stdlib.compare f2 f1)
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
                      test_result = RFailure ("Not finished", None, None);
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
                   Stdlib.compare t1.timestamp_start t2.timestamp_start)
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
      errors          = count is_error;
      failures        = count is_failure;
      skips           = count is_skip;
      todos           = count is_todo;
      timeouts        = count is_timeout;
      successes       = count is_success;
    }
