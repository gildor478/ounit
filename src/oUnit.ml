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

open OUnitUtils

let get_test_context,
    set_test_context,
    reset_test_context =
  let context_opt = ref None in
    (* get *)
    (fun () ->
       match !context_opt with
         | Some ctxt -> ctxt
         | None -> failwith "Function need to be called from inside a test."),
    (fun ctxt ->
       context_opt := Some ctxt),
    (fun _ ->
       context_opt := None)

type node = ListItem of int | Label of string

let node1_of_node =
  function
    | OUnitTest.ListItem i -> ListItem i
    | OUnitTest.Label s -> Label s

let node_of_node1 =
  function
    | ListItem i -> OUnitTest.ListItem i
    | Label s -> OUnitTest.Label s

type path = node list

let path1_of_path pth =
  List.map node1_of_node pth

type test_fun = unit -> unit

type test =
    TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

let rec test1_of_test =
  function
    | OUnitTest.TestCase (_, f) -> TestCase (fun () -> f (get_test_context ()))
    | OUnitTest.TestList lst -> TestList (List.map test1_of_test lst)
    | OUnitTest.TestLabel (str, tst) -> TestLabel (str, test1_of_test tst)

let rec test_of_test1 =
  function
    | TestCase f ->
        OUnitTest.TestCase
          (OUnitTest.Short,
           fun ctxt ->
             set_test_context ctxt;
             f ();
             reset_test_context ())
    | TestList lst -> OUnitTest.TestList (List.map test_of_test1 lst)
    | TestLabel (str, tst) ->  OUnitTest.TestLabel (str, test_of_test1 tst)

let rec ounit2_of_ounit1 =
  function
    | TestCase f ->
        OUnit2.test_case
          (fun ctxt ->
             set_test_context ctxt;
             f ();
             reset_test_context ())
    | TestList lst ->
        OUnit2.test_list (List.map ounit2_of_ounit1 lst)
    | TestLabel (lbl, test) ->
        OUnit2.( >: ) lbl (ounit2_of_ounit1 test)

type test_result =
    RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

let test_result1_of_test_result path rslt =
  let path1 =
    path1_of_path path
  in
  let rslt1 =
    match rslt with
     | OUnitTest.RSuccess ->
         RSuccess path1
     | OUnitTest.RFailure (str, _, _) ->
         RFailure (path1, str)
     | OUnitTest.RError (str, _) ->
         RError (path1, str)
     | OUnitTest.RSkip str ->
         RSkip (path1, str)
     | OUnitTest.RTodo str ->
         RTodo (path1, str)
     | OUnitTest.RTimeout test_length ->
         RError (path1,
                 (Printf.sprintf
                    "timeout after %.1fs."
                    (OUnitTest.delay_of_length test_length)))
  in
    rslt1


type test_event =
    EStart of path
  | EEnd of path
  | EResult of test_result

type test_results = test_result list

let list_result1_of_list_result =
  List.map
    (fun (pth, rslt, _) ->
       test_result1_of_test_result pth rslt)

let assert_failure =
  OUnitAssert.assert_failure

let assert_bool =
  OUnitAssert.assert_bool

let ( @? ) =
  OUnitAssert.assert_bool

let assert_string =
  OUnitAssert.assert_string

let assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ?(verbose=false) prg args =
  let ctxt =
    let ctxt = get_test_context () in
    let conf' = Hashtbl.copy ctxt.OUnitTest.conf in
      OUnitConf.set ~origin:"OUnit.assert_command" conf'
        "verbose" (string_of_bool verbose);
      {
        ctxt with
            OUnitTest.test_logger =
              OUnitLogger.Test.create
                (OUnitLoggerStd.std_logger conf' OUnitLogger.shard_default)
                ctxt.OUnitTest.path;
      }
  in
    OUnitAssert.assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ~ctxt
      prg args

let assert_equal ?cmp ?printer ?pp_diff ?msg a b =
  OUnitAssert.assert_equal ?cmp ?printer ?pp_diff ?msg a b

let assert_raises ?msg exc f =
  OUnitAssert.assert_raises ?msg exc f

let skip_if =
  OUnitAssert.skip_if

let todo =
  OUnitAssert.todo

let cmp_float ?epsilon f1 f2 =
  OUnitUtils.cmp_float ?epsilon f1 f2

let bracket pre f post () =
  OUnitTest.section_ctxt (get_test_context ())
    (fun ctxt ->
       let fixture =
         OUnitBracket.create
           (fun _ -> pre ())
           (fun fixture _ -> post fixture)
           ctxt
       in
       let () = f fixture in
         ())

let bracket_tmpfile ?prefix  ?suffix ?mode gen () =
  OUnitTest.section_ctxt (get_test_context ())
    (fun ctxt ->
       let fixture =
         OUnitBracket.bracket_tmpfile ?prefix  ?suffix ?mode ctxt
       in
         gen fixture)

let (>:) a b =
  test1_of_test (OUnitTest.(>:) a (test_of_test1 b))

let (>::) a b =
  test1_of_test (OUnitTest.(>::) a (fun _ -> b ()))

let (>:::) a b =
  test1_of_test (OUnitTest.(>:::) a (List.map test_of_test1 b))

let test_decorate g tst =
  test1_of_test
    (OUnitTest.test_decorate
       (fun f ->
          let f1 = (fun () -> f (get_test_context ())) in
          let f1' = g f1 in
            (fun ctxt ->
               set_test_context ctxt;
               f1' ();
               reset_test_context ()))
       (test_of_test1 tst))

let test_filter ?skip lst test =
  let res =
    OUnitTest.test_filter ?skip lst (test_of_test1 test)
  in
    match res with
      | Some tst -> Some (test1_of_test tst)
      | None -> None

let test_case_count tst =
  OUnitTest.test_case_count (test_of_test1 tst)

let string_of_node nd =
  OUnitTest.string_of_node (node_of_node1 nd)

let string_of_path pth =
  OUnitTest.string_of_path (List.map node_of_node1 pth)

let test_case_paths tst =
  let lst =
    OUnitTest.test_case_paths (test_of_test1 tst)
  in
    List.map
      (List.map node1_of_node)
      lst

let default_v1_conf ?(verbose=false) () =
  OUnitConf.default
    ~preset:
    [
      "chooser", "simple";
      "runner", "sequential";
      "results_style_1_X", "true";
      "verbose", (string_of_bool verbose);
      "output_file", "none";
    ]
    ()

let perform_test logger1 tst =
  let logger =
    OUnitLogger.fun_logger
      (function
        | {OUnitLogger.event = OUnitLogger.GlobalEvent _; _} ->
             ()
         | {OUnitLogger.event = OUnitLogger.TestEvent (path, test_event); _} ->
             begin
               let path1 =
                 path1_of_path path
               in
                 match test_event with
                   | OUnitLogger.EStart ->
                       logger1 (EStart path1)
                   | OUnitLogger.EEnd ->
                       logger1 (EEnd path1)
                   | OUnitLogger.EResult rslt ->
                       logger1 (EResult (test_result1_of_test_result path rslt))
                   | OUnitLogger.ELog _ | OUnitLogger.ELogRaw _ ->
                       ()
             end)
      ignore
  in
  let conf = default_v1_conf () in
    list_result1_of_list_result
      (OUnitCore.perform_test
         conf
         logger
         (snd (OUnitRunner.choice conf))
         (snd (OUnitChooser.choice conf))
         (test_of_test1 tst))

let run_test_tt ?verbose test =
  let conf = default_v1_conf ?verbose () in
  list_result1_of_list_result
    (OUnitCore.run_test_tt
       conf
       (OUnitLoggerStd.create conf OUnitLogger.shard_default)
       (snd (OUnitRunner.choice conf))
       (snd (OUnitChooser.choice conf))
       (test_of_test1 test))

let run_test_tt_main ?(arg_specs=[]) ?(set_verbose=ignore) suite =
  let suite = test_of_test1 suite in
  let only_test = ref [] in
  let list_test = ref false in
  let verbose = ref false in
  let specs =
    [
      "-verbose",
      Arg.Set verbose,
      " Rather than displaying dots while running the test, be more verbose.";

      "-only-test",
      Arg.String (fun str -> only_test := str :: !only_test),
      "path Run only the selected tests.";

      "-list-test",
      Arg.Set list_test,
      " List tests";
    ] @ arg_specs
  in
  let () =
    Arg.parse
      (Arg.align specs)
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " [options] [-only-test path]*")
  in
  let conf = default_v1_conf ~verbose:!verbose () in
    set_verbose (OUnitLoggerStd.verbose conf);
    if !list_test then
      begin
        List.iter
          (fun pth -> print_endline (OUnitTest.string_of_path pth))
          (OUnitTest.test_case_paths suite);
        []
      end
    else
      begin
        let nsuite =
          if !only_test = [] then
            suite
          else
            begin
              match OUnitTest.test_filter ~skip:true !only_test suite with
                | Some test ->
                    test
                | None ->
                    failwithf
                      "Filtering test %s lead to no tests."
                      (String.concat ", " !only_test)
            end
        in

        let test_results =
          OUnitCore.run_test_tt
            conf
            (OUnitLoggerStd.std_logger conf OUnitLogger.shard_default)
            (snd (OUnitRunner.choice conf))
            (snd (OUnitChooser.choice conf))
            nsuite
        in
        if not (OUnitResultSummary.was_successful test_results) then
          exit 1
        else
          list_result1_of_list_result test_results;
      end
