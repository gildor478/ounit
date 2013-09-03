
open OUnitTypes
open OUnitLogger
open OUnitTest
open OUnitResultSummary

let ocaml_position pos =
  Printf.sprintf
    "File \"%s\", line %d, characters 1-1:"
    pos.filename pos.line

(* TODO: deprecate in 2.1.0. *)
let results_style_1_X =
  OUnitConf.make_bool
    "results_style_1_X"
    false
    "Use OUnit 1.X results printer (will be deprecated in 2.1.0+)."

let format_event conf verbose log_event =
  match log_event.event with
    | GlobalEvent e ->
        begin
          match e with
            | GConf (k, v) ->
                if verbose then
                  Printf.sprintf "%s=%S\n" k v
                else
                  ""
            | GInfo str ->
                if verbose then
                  str^"\n"
                else
                  ""
            | GStart ->
                if verbose then
                  "Start testing.\n"
                else
                  ""
            | GEnd ->
                if verbose then
                  "End testing.\n"
                else
                  ""
            | GResults (running_time, results, test_case_count) ->
                let separator1 = String.make (Format.get_margin ()) '=' in
                let separator2 = String.make (Format.get_margin ()) '-' in
                let buf = Buffer.create 1024 in
                let bprintf fmt = Printf.bprintf buf fmt in
                let print_results =
                  List.iter
                    (fun (path, test_result, pos_opt) ->
                       if results_style_1_X conf then
                         begin
                           bprintf "%s\n%s: %s\n\n%s\n%s\n"
                             separator1
                             (result_flavour test_result)
                             (string_of_path path)
                             (result_msg test_result)
                             separator2
                         end
                       else
                         begin
                           bprintf "%s\n" separator1;
                           begin
                             match pos_opt with
                               | Some pos ->
                                   bprintf "%s\n" (ocaml_position pos)
                               | None ->
                                   ()
                           end;
                           bprintf "Error: %s\n\n"
                             (string_of_path path);
                           begin
                             match test_result with
                               | RFailure (_, Some backtrace)
                               | RError (_, Some backtrace) ->
                                   bprintf "%s\n" backtrace
                               | _ ->
                                   ()
                           end;
                           bprintf "%s\n" (result_msg test_result);
                           bprintf "%s\n" separator2;
                         end)
                in
                let filter f =
                  let lst =
                    List.filter
                      (fun (_, test_result, _) -> f test_result)
                      results
                  in
                    lst, List.length lst
                in
                let errors, nerrors     = filter is_error in
                let failures, nfailures = filter is_failure in
                let skips, nskips       = filter is_skip in
                let todos, ntodos       = filter is_todo in

                  if not verbose then
                    bprintf "\n";

                  print_results errors;
                  print_results failures;
                  bprintf "Ran: %d tests in: %.2f seconds.\n"
                    (List.length results) running_time;

                  (* Print final verdict *)
                  if was_successful results then
                    begin
                      if skips = [] then
                        bprintf "OK"
                      else
                        bprintf "OK: Cases: %d Skip: %d"
                          test_case_count nskips
                    end
                  else
                    begin
                      bprintf
                        "FAILED: Cases: %d Tried: %d Errors: %d \
                              Failures: %d Skip:%d Todo:%d"
                        test_case_count
                        (List.length results)
                        nerrors
                        nfailures
                        nskips
                        ntodos;
                    end;
                  bprintf "\n";
                  Buffer.contents buf
        end

    | TestEvent (path, e) ->
        begin
          let string_of_result =
            if verbose then
              function
                | RSuccess        -> "ok\n"
                | RFailure (_, _) -> "FAIL\n"
                | RError (_, _)   -> "ERROR\n"
                | RSkip _         -> "SKIP\n"
                | RTodo _         -> "TODO\n"
            else
              function
                | RSuccess        -> "."
                | RFailure (_, _) -> "F"
                | RError (_, _)   -> "E"
                | RSkip _         -> "S"
                | RTodo _         -> "T"
          in
            if verbose then
              match e with
                | EStart ->
                    Printf.sprintf "%s start\n" (string_of_path path)
                | EEnd ->
                    Printf.sprintf "%s end\n" (string_of_path path)
                | EResult result ->
                    string_of_result result
                | ELog (lvl, str) ->
                    let prefix =
                      match lvl with
                        | `Error -> "E"
                        | `Warning -> "W"
                        | `Info -> "I"
                    in
                      prefix^": "^str^"\n"
                | ELogRaw str ->
                    str
            else
              match e with
                | EStart _ | EEnd _ | ELog _ | ELogRaw _ -> ""
                | EResult result -> string_of_result result
        end

let file_logger conf fn =
  let chn = open_out fn in
  let line = ref 1 in
  let fwrite ev =
    let str =  format_event conf true ev in
    String.iter (function '\n' -> incr line | _ -> ()) str;
    output_string chn str;
    flush chn
  in
  let fpos () =
    Some { filename = fn; line = !line }
  in
  let fclose () =
    close_out chn
  in
    {
      fwrite = fwrite;
      fpos   = fpos;
      fclose = fclose;
    }

let verbose =
  OUnitConf.make_bool
    "verbose"
    false
    "Run test in verbose mode."

let display =
  OUnitConf.make_bool
    "display"
    true
    "Output logs on screen."

let std_logger conf =
  if display conf then
    let fwrite log_ev =
      print_string (format_event conf (verbose conf) log_ev);
      flush stdout
    in
      {
        fwrite = fwrite;
        fpos   = (fun () -> None);
        fclose = ignore;
      }
  else
    null_logger

let output_file =
  OUnitConf.make_string_opt
    "output_file"
    (Some (Filename.concat OUnitUtils.buildir "oUnit.log"))
    "Output verbose log in the given file."

let create conf =
  let std_logger=
    std_logger conf
  in
  let file_logger =
    match output_file conf with
      | Some fn ->
          file_logger conf fn
      | None ->
          null_logger
  in
    combine [std_logger; file_logger]
