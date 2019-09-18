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

(** Standard functions for plugin (register, choose). *)


type name = string

module type SETTINGS =
sig
  type t
  val name: name
  val conf_help: string
  val default_name: name
  val default_value: t
end

module Make(Settings: SETTINGS) =
struct
  let all = ref [0, (Settings.default_name, Settings.default_value)]

  let register name pref f =
    all := (pref, (name, f)) :: !all

  let of_name s =
    try
      List.assoc s (List.map snd !all)
    with Not_found ->
      OUnitUtils.failwithf "Unable to find %s '%s'." Settings.name s

  let choice =
    OUnitConf.make_enum
      Settings.name
      (fun () -> List.map snd !all)
      Settings.default_name
      Settings.conf_help

  let preset lst =
    let _, (default, _) =
      List.fold_left max (List.hd !all) (List.tl !all)
    in
      (Settings.name, default) :: lst

end
