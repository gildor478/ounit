(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                        *)
(* Copyright (C) 2010 OCamlCore SARL                                   *)
(*                                                                     *)
(* See LICENSE for details.                                            *)
(***********************************************************************)

open OUnitUtils
open OUnitTypes

(*
 * Types and global states.
 *)

(* TODO: move into run_test_tt_main. *)
let global_verbose = 
  OUnitConf.make 
    "verbose"
    (fun r -> Arg.Set r)
    ~printer:string_of_bool
    false
    "Run test in verbose mode."

let global_output_file = 
  let pwd = Sys.getcwd () in
  let ocamlbuild_dir = Filename.concat pwd "_build" in
  let dir = 
    if Sys.file_exists ocamlbuild_dir && Sys.is_directory ocamlbuild_dir then
      ocamlbuild_dir
    else 
      pwd
  in
  let fn = Filename.concat dir "oUnit.log" in
    OUnitConf.make
      "output_file"
      ~arg_string:"fn"
      ~alternates:["no_output_file",
                   (fun r -> Arg.Unit (fun () -> r:= None)),
                   None,
                   "Prevent to write log in a file."]
      ~printer:(function 
                  | None -> "<none>"
                  | Some fn -> Printf.sprintf "%S" fn)
      (fun r -> Arg.String (fun s -> r := Some s))
      (Some fn)
      "Output verbose log in the given file."

(* TODO: remove *)
let global_chooser = ref OUnitChooser.simple

(* TODO: remove *)
let global_runner = ref OUnitRunnerSeq.run_all_tests

(* Run all tests, report starts, errors, failures, and return the results *)
let perform_test logger test =
  let rec flatten_test path acc = 
    function
      | TestCase(f) -> 
          (path, f) :: acc

      | TestList (tests) ->
          fold_lefti 
            (fun acc t cnt -> 
               flatten_test 
                 ((ListItem cnt)::path) 
                 acc t)
            acc tests
      | TestLabel (label, t) -> 
          flatten_test ((Label label)::path) acc t
  in
  let test_cases = 
    List.rev (flatten_test [] [] test) 
  in
    !global_runner logger !global_chooser test_cases

(* A simple (currently too simple) text based test runner *)
let run_test_tt ?verbose test =
  let () =
    Printexc.record_backtrace true
  in

  let base_logger = 
    OUnitLogger.create 
      (global_output_file ())
      (global_verbose ())
      OUnitLogger.null_logger
  in
  let html_logger =
    OUnitLoggerHTML.create ()
  in
  let junit_logger =
    OUnitLoggerJUnit.create ()
  in
  let logger =
    OUnitLogger.combine
      [base_logger; html_logger; junit_logger]
  in

  let () =
    OUnitConf.dump (OUnitLogger.report logger)
  in

  (* Now start the test *)
  let running_time, test_results = 
    time_fun 
      perform_test
      logger
      test 
  in
    
    (* Print test report *)
    OUnitLogger.report logger 
      (GlobalEvent 
         (GResults (running_time, test_results, test_case_count test)));

    (* Reset logger. *)
    OUnitLogger.close logger;

    (* Return the results possibly for further processing *)
    test_results
      
(* Call this one from you test suites *)
let run_test_tt_main ?(arg_specs=[]) ?(set_verbose=ignore) ?fexit suite = 
  let fexit = 
    match fexit with 
      | Some f -> f 
      | None ->
          (fun test_results ->
             if not (was_successful test_results) then
               exit 1)
  in
  let only_test =
    OUnitConf.make 
      "only_test"
      ~arg_string:"path"
      ~printer:(fun lst -> String.concat "," (List.map (Printf.sprintf "%S") lst))
      (fun r -> Arg.String (fun str -> r := str :: !r))
      []
      "Run only the selected tests."
  in
  let list_test =
    OUnitConf.make
      "list_test"
      (fun r -> Arg.Set r)
      ~printer:string_of_bool 
      false
      "List tests"
  in
  let () = 
    OUnitConf.load arg_specs
  in
    if list_test () then
      begin
        List.iter
          (fun pth -> print_endline (string_of_path pth))
          (OUnitTest.test_case_paths suite)
      end
    else
      begin
        let nsuite = 
          if only_test () = [] then
            suite
          else
            begin
              match OUnitTest.test_filter ~skip:true (only_test ()) suite with 
                | Some test ->
                    test
                | None ->
                    failwith 
                      (Printf.sprintf
                         "Filtering test %s lead to no tests."
                         (String.concat ", " (only_test ())))
            end
        in

        let test_results = 
          set_verbose (global_verbose ());
          run_test_tt ~verbose:(global_verbose ()) nsuite 
        in
          fexit test_results
      end
