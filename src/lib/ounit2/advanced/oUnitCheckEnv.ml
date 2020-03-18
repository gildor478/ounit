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


(* Check environment after and before tests, to check isolation. *)

open OUnitTest
open OUnitAssert

type t =
  {
    pwd: string;
    env: string array;
  }

let create () =
  {
    pwd = Sys.getcwd ();
    env =
      let e = Unix.environment () in
      if Sys.os_type = "Win32" then begin
        let lst =
          Array.fold_right
            (fun v lst ->
              (* On Win32, sometimes an environment variable like:
                 "=C:=C:\\foobar" will be added. AFAIU, this is the absolute
                 location in the drive C: and it helps to resolve relative root
                 path. For example, "C:" which is relative will translate to
                 "C:\\foobar" in this case.

                 We don't take this into account because using "chdir" elsewhere
                 will change this value in the environment.

                 https://devblogs.microsoft.com/oldnewthing/20100506-00/?p=14133
               *)
              if OUnitUtils.starts_with ~prefix:"=" v then
                lst
              else
                v :: lst)
            e []
        in
        Array.of_list lst
      end else begin
        e
      end;
  }

module EnvElement =
struct
  type t = string

  let pp_printer = Format.pp_print_string

  let compare = String.compare

  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module SetEnv = OUnitDiff.SetMake(EnvElement)

let check test_ctxt t =
  let t' = create () in
    List.iter
      (fun f -> non_fatal test_ctxt (fun _ -> f ()))
      [
        (fun () ->
           assert_equal
             ~msg:"Check that the current working dir hasn't changed during the \
             test."
             ~printer:(fun s -> s)
             t.pwd
             t'.pwd);
        (fun () ->
           let convert t = SetEnv.of_list (Array.to_list t.env) in
             SetEnv.assert_equal
               ~msg:"Check that the environment variables haven't changed during \
               the test."
               (convert t)
               (convert t'));
      ]
