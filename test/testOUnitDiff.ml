
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

let test_diff ctxt =
  let lst_exp =
    [1; 2; 3; 4; 5]
  in
  let lst_real =
    [1; 2; 5; 4]
  in
    assert_raises
      (Failure "OUnit: expected: 1, 2, 3, 4, 5 but got: 1, 2, 4, 5\n\
                differences: -3")
      (fun () ->
         DiffSetInt.assert_equal
           (DiffSetInt.of_list lst_exp)
           (DiffSetInt.of_list lst_real));
    DiffSetInt.assert_equal
      (DiffSetInt.of_list lst_exp) (DiffSetInt.of_list lst_exp);
    assert_raises
      (Failure "OUnit: expected: 1, 2, 3, 4, 5 but got: 1, 2, 5, 4\
                \ndifferences: element number 2 differ (3 <> 5)")
      (fun () ->
         DiffListSimpleInt.assert_equal lst_exp lst_real);
    DiffListSimpleInt.assert_equal lst_exp lst_exp


(* Construct the test suite *)
let tests =
  "OUnitDiff" >:::
  ["test_diff" >:: test_diff]
