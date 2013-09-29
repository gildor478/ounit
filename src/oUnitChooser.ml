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

(**
    Heuristic to pick a test to run.

    @author Sylvain Le Gall
  *)

open OUnitTest

type t =
    {
      tests_planned: path list;
      tests_running: path list;
      tests_passed: result_list;
      cache: OUnitCache.cache;
    }

type choice =
  | ChooseToSkip of path
  | ChooseToPostpone
  | Choose of path
  | NoChoice

let string_of_choice =
  function
    | ChooseToSkip path ->
        Printf.sprintf "ChooseToSkip %S" (string_of_path path)
    | ChooseToPostpone -> "ChooseToPostpone"
    | Choose path ->
        Printf.sprintf "Choose %S" (string_of_path path)
    | NoChoice -> "NoChoice"


type chooser = t -> choice

(** Most simple heuristic, just pick the first test. *)
let simple t =
  match t.tests_planned with
    | hd :: _ -> Choose hd
    | [] -> NoChoice

module Plugin =
  OUnitPlugin.Make
    (struct
       type t = chooser
       let name = "chooser"
       let conf_help =
         "Select the method to choose tests to run."
       let default_name = "simple"
       let default_value = simple
     end)

include Plugin

let allskip t =
  match t.tests_planned with
    | hd :: _ -> ChooseToSkip hd
    | [] -> NoChoice

let failfirst t =
  let was_successful = OUnitResultSummary.was_successful in
  let rec find_failing =
    function
      | path :: tl ->
          begin
            match OUnitCache.get_result path t.cache with
              | Some result ->
                  (* Find the first formerly failing test. *)
                  if was_successful [path, result, None] then
                    find_failing tl
                  else
                    Choose path
              | None ->
                  Choose path
          end
      | [] ->
          begin
            let wait_results_running =
              List.fold_left
                (fun wait path ->
                   match OUnitCache.get_result path t.cache with
                     | Some result ->
                         (not (was_successful [path, result, None])) || wait
                     | None ->
                         (* No former result, we need the result of
                          * this test.
                          *)
                         true)
                false t.tests_running
            in
              if wait_results_running then
                (* We need more data about currently running tests. *)
                ChooseToPostpone
              else if was_successful t.tests_passed then
                (* All tests that were red has become green, continue. *)
                simple t
              else
                (* Some tests still fail, skip the rest. *)
                allskip t
          end
  in
    find_failing t.tests_planned

let () =
  register "failfirst" ~-1 failfirst
