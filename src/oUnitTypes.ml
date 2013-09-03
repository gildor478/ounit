
(**
  * Commont types for OUnit
  *
  * @author Sylvain Le Gall
  *
  *)

exception Skip of string

exception Todo of string

(** See OUnit.mli. *)
type node = ListItem of int | Label of string

(** See OUnit.mli. *)
type path = node list

(** See OUnit2.mli. *)
type backtrace = string option

(** See OUnit.mli. *)
type test_result =
  | RSuccess
  | RFailure of string * backtrace
  | RError of string * backtrace
  | RSkip of string
  | RTodo of string

(* See OUnit.mli. *)
type position =
    {
      filename: string;
      line: int;
    }

(* See OUnit.mli. *)
type test_result_full = (path * test_result * position option)
type test_results = test_result_full list

