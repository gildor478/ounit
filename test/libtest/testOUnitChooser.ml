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
open OUnitChooser
open OUnitTest

module MapString = Map.Make(String)

let choose label = Choose [Label label]
let postpone = ChooseToPostpone
let skip label = ChooseToSkip [Label label]

let check_choices chooser tests expected_choices =
  let result_of_test =
    List.fold_left
      (fun mp (path, _, result) ->
         MapString.add path result mp)
      MapString.empty tests
  in
  let add_result path result t =
    {t with
         tests_passed = (path, result, None) :: t.tests_passed}
  in
  let not_planned path t =
    {t with tests_planned = List.filter ((<>) path) t.tests_planned}
  in
  let rec virtual_run choices t =
    (* Choose with 1+ test still running. *)
    let choice = chooser t in
    (* Finish the running test. *)
    let t =
      match t.tests_running with
        | path :: tl ->
            let result =
              MapString.find (string_of_path path) result_of_test
            in
            let t = add_result path result t in
              {t with tests_running = tl}
        | [] ->
            t
    in
    (* Apply the choice. *)
    let choices = choice :: choices in
      match choice with
        | ChooseToSkip path ->
            virtual_run choices
              (not_planned path (add_result path (RSkip "") t))
        | ChooseToPostpone ->
            virtual_run choices t
        | Choose path ->
            virtual_run choices
              (not_planned path
                 {t with tests_running = path :: t.tests_running})
        | NoChoice ->
            choices, t
  in
  let t =
    {
      tests_planned = List.map (fun (path, _, _) -> [Label path]) tests;
      tests_running = [];
      tests_passed = [];
      cache = List.fold_left
                (fun cache (path, was_result_opt, _) ->
                   match was_result_opt with
                     | Some result ->
                         OUnitCache.add_result [Label path] result cache
                     | None ->
                         cache)
                OUnitCache.default tests
    }
  in
  let actual_choices, t = virtual_run [] t in
    assert_equal
      ~msg:"All tests passed."
      ~printer:string_of_int
      (List.length tests)
      (List.length t.tests_passed);
    assert_equal
      ~msg:"Right choices made."
      ~printer:(fun choices ->
                  String.concat ", " (List.map string_of_choice choices))
      (expected_choices @ [NoChoice])
      (List.rev actual_choices)

let test ?(run=true) ?(failed=false) ?(still=true) label =
  label,
  begin
    if run && failed then
      Some (RFailure ("", None, None))
    else if run then
      Some RSuccess
    else
      None
  end,
  begin
    let now_failed = if still then failed else not failed in
      if now_failed then
        RFailure ("", None, None)
      else
        RSuccess
  end

let tests =
  "Chooser" >:::
  [
    "failfirst" >::
    (fun _ ->
       check_choices failfirst
         [test "foo"]
         [choose "foo"];
       check_choices failfirst
         [test "foo"; test ~failed:true ~still:false "bar"]
         [choose "bar"; postpone; choose "foo"];
       check_choices failfirst
         [test "foo"; test ~failed:true "bar"]
         [choose "bar"; postpone; skip "foo"])
  ]
