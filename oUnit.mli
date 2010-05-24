(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright 2002, 2003 Maas-Maarten Zeeman. All rights reserved. See  *) 
(* LICENCE for details.                                                *)
(***********************************************************************)

(* $Id: oUnit.mli,v 1.10 2003/02/27 21:49:06 maas Exp $ *)

(** The OUnit library can be used to implement unittests

    To uses this library link with
      [ocamlc oUnit.cmo]
    or 
      [ocamlopt oUnit.cmx]
 
    @author Maas-Maarten Zeeman
*)

(** {5 Assertions} 

    Assertions are the basic building blocks of unittests. *)

(** Signals a failure. This will raise an exception with the specified
    string. 

    @raise Failure to signal a failure *)
val assert_failure : string -> 'a

(** Signals a failure when bool is false. The string identifies the 
    failure.
    
    @raise Failure to signal a failure *)
val assert_bool : string -> bool -> unit

(** Shorthand for assert_bool 

    @raise Failure to signal a failure *)
val ( @? ) : string -> bool -> unit

(** Signals a failure when the string is empty. The string identifies the
    failure 
    
    @raise Failure to signal a failure *) 
val assert_string : string -> unit

(** Compares two values, when they are not equal a failure is signaled.
    The optional printer can be used to convert the value to string, so
    a nice error message can be formatted. When msg is also set it can
    be used to identify the failure 

    @raise Failure description *)
val assert_equal : ?printer:('a -> string) -> ?msg:string -> 'a -> 'a -> unit

(** Asserts if the expected exception was raised. When msg is set it can 
    be used to identify the failure

    @raise Failure description *)
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit

(** {5 Constructing Tests} *)

(** The type of tests *)
type test =
    TestCase of (unit -> unit)
  | TestList of test list
  | TestLabel of string * test

(** Create a TestLabel for a test *)
val (>:) : string -> test -> test

(** Create a TestLabel for a TestCase *)
val (>::) : string -> (unit -> unit) -> test

(** Create a TestLabel for a TestList *)
val (>:::) : string -> test list -> test

(** Some shorthands which allows easy test construction.

   Examples:

   - ["test1" >: TestCase((fun _ -> ()))] =>  
   [TestLabel("test2", TestCase((fun _ -> ())))]
   - ["test2" >:: (fun _ -> ())] => 
   [TestLabel("test2", TestCase((fun _ -> ())))]

   - ["test-suite" >::: ["test2" >:: (fun _ -> ());]] =>
   [TestLabel("test-suite", TestSuite([TestLabel("test2", TestCase((fun _ -> ())))]))]
*)

(** {5 Retrieve Information from Tests} *)

(** Returns the number of available test cases *)
val test_case_count : test -> int

(** Types needed to represent the path of a test *)

type node = ListItem of int | Label of string
type path = node list (** The path to the test (in reverse order). *)

(** Make a string form a node *)
val string_of_node : node -> string

(** Make a string from a path. The path will be reversed before it is 
    tranlated into a string *)
val string_of_path : path -> string

(** Returns a list with paths of the test *)
val test_case_paths : test -> path list

(** Counts *)
type counts = {cases : int; tried : int; errors : int; failures : int;}

(** Returns true if the counts indicate that the test run was 
    successful. This means that the errors and failures must be 0 *)
val was_successful : counts -> bool

(** {5 Performing Tests} 

    TODO
    *)

(** Events which can occur during a test *)
type test_event =
    EStart of path * counts (** Indicates the start of a test-case *)
  | EEnd of path * counts (** Indicates the end of a test-case *)
  | ESuccess of path * counts (** Indicates success of the test-case *)
  | EFailure of path * string * counts (** Indicates an failure has occurred *)
  | EError of path * string * counts (** Indicates that an error has occurred *)

(** Perform the test, allows you to build your own test runner *)
val perform_test : (test_event -> 'a) -> test -> counts

(** A simple text based test runner. It prints out information
    during the test. *)
val run_test_tt : ?verbose:bool -> test -> counts

(** Main version of the text based test runner. It reads the supplied command 
    line arguments to set the verbose level *)
val run_test_tt_main : test -> counts



