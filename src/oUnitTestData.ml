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

let make_filename = List.fold_left Filename.concat

let testdata_default =
  let pwd = Sys.getcwd () in
  let is_dir lst =
    let dn = make_filename pwd lst in
      Sys.file_exists dn && Sys.is_directory dn
  in
    try
      let path =
        List.find is_dir
          [
            ["test"; "data"];
            ["tests"; "data"];
            ["data"]
          ]
      in
        Some (make_filename pwd path)
    with Not_found ->
      None

let testdata_dir =
  OUnitConf.make_string_opt
    "testdata_dir"
    testdata_default
    "Location of the test data directory (absolute path)."

let in_testdata_dir conf path =
  match testdata_dir conf with
    | Some fn -> make_filename fn path
    | None ->
        failwith "Test data dir not defined."
