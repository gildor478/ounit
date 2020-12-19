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
open OUnitConf

type t =
    {
      vint: OUnitConf.conf -> int;
      vstring: OUnitConf.conf -> string;
    }

let bracket_ounitconf =
  bracket
    (fun _ ->
       (* TODO: we need a lock here. *)
       {
         vint = make_int "int" 0 "";
         vstring = make_string "string" "" "";
       })
    (fun _ _ ->
       Hashtbl.remove metaconf "int";
       Hashtbl.remove metaconf "string";
       (* TODO: release the lock. *)
       ())


let tests =
  "OUnitConf" >:::
  [
    "CLI" >::
    (fun test_ctxt ->
       let t = bracket_ounitconf test_ctxt in
       let conf =
         load
           ~argv:[|"foo"; "-int"; "2"; "-string"; "foo bar"|]
           []
       in
         assert_equal ~printer:string_of_int 2 (t.vint conf);
         assert_equal ~printer:(fun s -> s) "foo bar" (t.vstring conf));

    "File" >::
    (fun test_ctxt ->
       let fn, chn = bracket_tmpfile test_ctxt in
       let t = bracket_ounitconf test_ctxt in
       let () =
         output_string chn
           "int = 1\n\
            string = \"abcd ef\"";
         close_out chn
       in
       let conf = load ~argv:[|"foo"; "-conf"; fn|] [] in
         assert_equal ~printer:string_of_int 1 (t.vint conf);
         assert_equal ~printer:(fun s -> s) "abcd ef" (t.vstring conf));

    "Substitution" >::
    (fun test_ctxt ->
       let _ = bracket_ounitconf test_ctxt in
       let conf = load ~argv:[|"foo"; "-int"; "10"|] [] in
         assert_equal
           ~printer:(fun s -> s)
           "foo-10"
           (subst conf [] "foo-$int"));

    "NoDoubleInject" >::
    (fun test_ctxt ->
       let _ = bracket_ounitconf test_ctxt in
         try
           let _option: conf -> string  = make_string "string" "" "" in
             assert_failure
               "Should not be able to inject duplicate configuration \
                option 'string'."
         with Failure _ ->
           ());
  ]
