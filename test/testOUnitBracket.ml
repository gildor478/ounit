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
