
open OUnit2

let xmllint =
  conf_make
    "xmllint"
    (fun v -> Arg.Set_string v)
    ~printer:(fun s -> s)
    "xmllint"
    "XML linter program to validate output."

let testFakeHTML =
  conf_make
    "testFakeHTML"
    (fun v -> Arg.Set_string v)
    ~printer:(fun s -> s)
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
         ())
  ]
