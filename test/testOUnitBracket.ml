
open OUnit2
open OUnitBracket

(* Retain bracket return for further testing. *)
let with_bracket_holder test_ctxt bracket f =
  let rres = ref None in
    with_bracket test_ctxt bracket
      (fun res _ ->
         rres := Some res;
         f res);
    match !rres with
      | None ->
          assert_failure "Bracket holder not initialized."
      | Some res ->
          res


let tests =
  "OUnitBracket" >:::
  [
    "tmpfile" >::
    (fun test_ctxt ->
       let fn, _ =
         with_bracket_holder
           test_ctxt bracket_tmpfile
           (fun (fn, _) ->
              assert_bool
                "Temporary file exists."
                (Sys.file_exists fn))
       in
         assert_bool
           "Temporary file doesn't exist anymore."
           (not (Sys.file_exists fn)));

    "tmpdir" >::
    (fun test_ctxt ->
       let dn =
         with_bracket_holder
           test_ctxt bracket_tmpdir
           (fun dn ->
              assert_bool
                "Temporary directory exists."
                (Sys.is_directory dn))
       in
         assert_bool
           "Temporary directory doesn't exist anymore."
           (not (Sys.file_exists dn)));

    "chdir" >::
    (fun test_ctxt ->
       let tmpdn = bracket_tmpdir test_ctxt in
       let () =
         with_bracket
           test_ctxt (bracket_chdir tmpdn)
           (fun () (test_ctxt : OUnitTest.ctxt) ->
              assert_equal
                ~printer:(fun s -> s)
                tmpdn
                (Sys.getcwd ()))
       in
         assert_bool
           "Not in temporary directory anymore."
           (tmpdn <> Sys.getcwd ()));

  ]
