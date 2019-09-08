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

open OUnitTest
open OUnit2

let perform_test test =
  let null_logger = OUnitLogger.null_logger in
  let conf = OUnitConf.default () in
    OUnitCore.perform_test
      conf
      null_logger
      OUnitRunner.sequential_runner
      OUnitChooser.simple
      test

let assert_equal_test_result exp res =
  let norm lst =
   let norm_one (path, test_result, pos) =
     let test_result' =
       match test_result with
         | RSuccess -> RSuccess
         | RFailure (str, _, _) -> RFailure (str, None, None)
         | RError (str, _) -> RError(str, None)
         | RSkip str -> RSkip str
         | RTodo str -> RTodo str
         | RTimeout test_length -> RTimeout test_length
     in
       (path, test_result', pos)
   in
     List.sort Stdlib.compare (List.rev_map norm_one lst)
  in
  assert_equal
    ~cmp:
    (fun a b -> norm a = norm b)
    ~printer:
    (fun results ->
      String.concat "; "
        (List.map
           (fun (path, test_result, _) ->
              let spf fmt = Printf.sprintf fmt in
              let string_of_backtrace =
                function
                  | Some str -> spf "Some (%S)" str
                  | None -> "None"
              in
              let test_result_string =
                match test_result with
                  | RSuccess ->
                      "RSuccess"
                  | RFailure (str, _, backtrace) ->
                      spf "RFailure(%S, _, %s)"
                        str (string_of_backtrace backtrace)
                  | RError (str, backtrace) ->
                      spf "RError(%S, %s)" str (string_of_backtrace backtrace)
                  | RSkip str ->
                      spf "RSkip(%S)" str
                  | RTodo str ->
                      spf "RTodo(%S)" str
                  | RTimeout _ ->
                      "RTimeout(_)"
              in
                Printf.sprintf "%S, %s"
                  (OUnitTest.string_of_path path) test_result_string)
           (norm results)))
    exp res

let skip_if_notunix () = skip_if (Sys.os_type <> "Unix") "Only run on Unix."
