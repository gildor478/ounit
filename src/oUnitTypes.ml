
(**
  * Commont types for OUnit
  *
  * @author Sylvain Le Gall
  *
  *)

(** See OUnit.mli. *) 
type node = ListItem of int | Label of string

(** See OUnit.mli. *) 
type path = node list 

(** See OUnit.mli. *) 
type test_result =
  | RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

(* See OUnit.mli. *)
type position =
    {
      filename: string;
      line: int;
    }

(* See OUnit.mli. *)
type test_results = (test_result * position option) list

(** See OUnit.mli. *) 
type log_severity = 
  | LError
  | LWarning
  | LInfo

(** See OUnit.mli. *) 
type test_event =
  | EStart of path
  | EEnd of path
  | EResult of test_result
  | ELog of log_severity * string
  | ELogRaw of string

(** Events which occur at the global level. *)
type global_event =
  | GConf of string (** Dump a configuration options. *)
  | GStart  (** Start running the tests. *)
  | GEnd    (** Finish running the tests. *)
  | GResults of (float * test_results * int)

type event_type = 
  | GlobalEvent of global_event
  | TestEvent of test_event

(* The type of test function *)
type test_fun = unit -> unit 

(* The type of tests *)
type test = 
  | TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

type state = 
    {
      tests_planned: (path * (unit -> unit)) list;
      results: test_results;
    }
