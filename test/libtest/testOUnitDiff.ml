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

module EInt =
struct
  type t = int
  let compare = ( - )
  let pp_printer = Format.pp_print_int
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module DiffSetInt = OUnitDiff.SetMake(EInt)

module DiffListSimpleInt = OUnitDiff.ListSimpleMake(EInt)

let test_diff _ =
  let lst_exp =
    [1; 2; 3; 4; 5]
  in
  let lst_real =
    [1; 2; 5; 4]
  in
    assert_raises
      (OUnitTest.OUnit_failure "expected: 1, 2, 3, 4, 5 but got: 1, 2, 4, 5\n\
                                differences: -3")
      (fun () ->
         DiffSetInt.assert_equal
           (DiffSetInt.of_list lst_exp)
           (DiffSetInt.of_list lst_real));
    DiffSetInt.assert_equal
      (DiffSetInt.of_list lst_exp) (DiffSetInt.of_list lst_exp);
    assert_raises
      (OUnitTest.OUnit_failure "expected: 1, 2, 3, 4, 5 but got: 1, 2, 5, 4\n\
                                differences: element number 2 differ (3 <> 5)")
      (fun () ->
         DiffListSimpleInt.assert_equal lst_exp lst_real);
    DiffListSimpleInt.assert_equal lst_exp lst_exp


(* Construct the test suite *)
let tests =
  "OUnitDiff" >:::
  ["test_diff" >:: test_diff]
