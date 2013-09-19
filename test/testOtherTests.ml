
open OUnit2

let xmllint =
  Conf.make_string
    "xmllint"
    "xmllint"
    "XML linter program to validate output."

let testFakeHTML =
  Conf.make_string
    "testFakeHTML"
    "testFakeHTML"
    "The testFakeHTML executable to run."

let tests =
  "OtherTests" >:::
  [
    "TestFakeHTML" >::
    (fun ctxt ->
       (* For easier dev. we don't use a temporary directory but a permanent
        * one, so that we can see the result.
        *)
       let () =
         skip_if (Sys.os_type = "Win32")
           "Don't run on Win32."
       in
       let html_dir = "log-html" in
       let junit_xml = Filename.concat html_dir "junit.xml" in
       let link_to_source bn =
         Sys.remove (Filename.concat html_dir bn);
         Unix.symlink
           (Filename.concat (Sys.getcwd ()) (Filename.concat "src" bn))
           (Filename.concat html_dir bn)
       in
         if not (Sys.file_exists html_dir) then
           Unix.mkdir html_dir 0o750;
         assert_command
           ~ctxt
           ~exit_code:(Unix.WEXITED 1)
           (testFakeHTML ctxt)
           ["-output-file"; Filename.concat html_dir "fake-html.log";
            "-output-html-dir"; html_dir;
            "-output-junit-file"; junit_xml];
         (* Fixing some files to use source version of it. *)
         List.iter link_to_source ["oUnit.js"; "oUnit.css"];
         assert_command
           ~ctxt
           (xmllint ctxt)
           ["--noout"; "--nonet"; "--schema"; "test/JUnit.xsd"; junit_xml];
         (* TODO: css validation and xhtml validation. *)
         ());

      "BacktraceProcessing" >::
      (fun ctxt ->

         List.iter
           (fun (str, exp) ->
              let lst = OUnitUtils.extract_backtrace_position str in
                assert_equal
                  ~printer:(fun lst ->
                              String.concat "; "
                                (List.map
                                   (function
                                      | None -> "None"
                                      | Some (f,l) ->
                                          Printf.sprintf "%S, %d" f l)
                                   lst))
                  [exp] lst)
           [
             "Raised at unknown location",
             None;

             "Raised at file \"src/oUnitAssert.ml\", line 14, characters 8-27",
             Some ("src/oUnitAssert.ml", 14);

             "Called from file \"test/testOtherTests.ml\", line 67, \
              characters 49-104",
             Some ("test/testOtherTests.ml", 67);

             "Called from file \"src/oUnitRunner.ml\", line 14, \
              characters 11-24",
             Some ("src/oUnitRunner.ml", 14);
           ]);


      "AssertCodePosition" >::
      (fun ctxt ->
         skip_if (not (Printexc.backtrace_status ())) "No backtrace.";
         let extract_exc e =
           let _, result, _ = OUnitTest.result_full_of_exception ctxt e in
           match result with
             | OUnitTest.RFailure (str,
                                   Some {OUnitLogger.filename = fn;
                                         line = lineno}, _) ->
                 fn, lineno
             | e ->
                 assert_failure "Should return a position."
         in

         (* Keep the following two assert 3 lines away, *)
         let fn1, lineno1 =
           try assert_equal 1 2; "", 0 with e -> extract_exc e
         in
         let fn2, lineno2 =
           try assert_equal 2 1; "", 0 with e -> extract_exc e
         in
         let fn_exp = "test/testOtherTests.ml" in
           assert_equal ~printer:(fun s -> s) fn_exp fn1;
           assert_equal ~printer:(fun s -> s) fn_exp fn2;
           assert_equal ~printer:string_of_int 3 (lineno2 - lineno1));
  ]
