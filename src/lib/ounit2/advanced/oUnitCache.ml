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

type cache = OUnitTest.result MapPath.t

let cache_filename =
  OUnitConf.make_string_subst_opt
    "cache_filename"
    (* TODO: oUnit-$(name).cache *)
    (Some (Filename.concat OUnitUtils.buildir "oUnit-$(suite_name).cache"))
    "Cache file to store previous results."

let default = MapPath.empty

let load conf =
  match cache_filename conf with
    | Some fn ->
        begin
          try
            let chn = open_in_bin fn in
            let cache : cache =
              try
                Marshal.from_channel chn
              with _ ->
                default
            in
              close_in chn;
              cache
          with _ ->
            default
        end

    | None ->
        default

let dump conf cache =
  match cache_filename conf with
    | Some fn ->
        begin
          try
            let chn = open_out_bin fn in
              Marshal.to_channel chn cache [];
              close_out chn
          with _ ->
            ()
        end

    | None ->
        ()

let get_result path cache =
  try
    Some (MapPath.find path cache)
  with Not_found ->
    None

let add_result path result cache =
  MapPath.add path result cache
