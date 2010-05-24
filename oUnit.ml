(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright 2002, 2003, 2004 Maas-Maarten Zeeman. All rights          *)
(* reserved. See                                                       *) 
(* LICENCE for details.                                                *)
(***********************************************************************)

(* $Id: oUnit.ml,v 1.15 2004/07/24 09:40:58 maas Exp $ *)

let bracket set_up f tear_down () =
  let fixture = set_up () in
  try
    f fixture;
    tear_down fixture
  with
    e -> 
      tear_down fixture;
      raise e

let assert_failure msg = failwith ("OUnit: " ^ msg)

let assert_bool msg b =
  if b then ()
  else
    assert_failure msg

let assert_string s =
  if s = "" then ()
  else
    assert_failure s

let assert_equal ?(cmp = ( = )) ?printer ?msg expected actual  =
  if cmp expected actual then ()
  else
    match printer, msg with 
      None, None -> 
	assert_failure "not equal"
    | None, Some s -> 
	assert_failure (Format.sprintf "@[%s@\nnot equal@]" s)
    | Some p, None ->
	assert_failure 
	  (Format.sprintf "expected: %s but got: %s" (p expected) (p actual))
    | Some p, Some s -> 
	assert_failure 
	  (Format.sprintf "@[%s@\nexpected: %s but got: %s@]" 
	     s (p expected) (p actual))

let assert_raises ?msg exn (f: unit -> 'a) = 
  let pexn = Printexc.to_string in
  let expected_exception_raised = 
    try 
      f (); false
    with
    | e -> assert_equal ?msg ~printer:pexn exn e; true
  in
  if not expected_exception_raised then 
    let err_str = 
      Format.sprintf "expected exception %s, but no exception was not raised." 
	(pexn exn)
    in
    match msg with
      None ->
	assert_failure err_str
    | Some s ->
	assert_failure 
	  (Format.sprintf "@[%s@\n%s@]" s err_str)

(* Compare floats up to a given relative error *)
let cmp_float ?(epsilon = 0.00001) a b =
  abs_float (a -. b) <= epsilon *. (abs_float a) ||
  abs_float (a -. b) <= epsilon *. (abs_float b) 
      
(* Now some handy shorthands *)
let (@?) = assert_bool

(* The type of tests *)
type test = 
    TestCase of (unit -> unit)
  | TestList of test list
  | TestLabel of string * test

(* Some shorthands which allows easy test construction *)
let (>:) s t = TestLabel(s, t)             (* infix *)
let (>::) s f = TestLabel(s, TestCase(f))  (* infix *)
let (>:::) s l = TestLabel(s, TestList(l)) (* infix *)

(* Return the number of available tests *)
let rec test_case_count = function
    TestCase(_) -> 1
  | TestLabel(_, t) -> test_case_count t
  | TestList(l) -> List.fold_left (fun c t -> c + test_case_count t) 0 l

type node = ListItem of int | Label of string
type path = node list

let string_of_node = function
    ListItem n -> (string_of_int n)
  | Label s -> s

let string_of_path path =
  List.fold_left 
    (fun a l -> 
      if a = "" then l
      else l ^ ":" ^ a) "" (List.map string_of_node path)
    
(* Some helper function, they are generally applicable *)
(* Applies function f in turn to each element in list. Function f takes
   one element, and integer indicating its location in the list *)
let mapi f = 
  let rec rmapi i = function [] -> [] | h::t -> (f h i)::(rmapi (i + 1) t) in
  rmapi 0

let fold_lefti f accu =
  let rec rfold_lefti i accup = function
      [] -> accup
    | a::l -> rfold_lefti (i + 1) (f accup a i) l
  in
  rfold_lefti 0 accu

(* Returns all possible paths in the test. The order is from test case
   to root 
*)
let test_case_paths test = 
  let rec tcps path = function
      TestCase(_) -> [path] 
    | TestList(tests) -> 
	List.concat (mapi (fun t i -> tcps ((ListItem i)::path) t) tests)
    | TestLabel(l, t) -> tcps ((Label l)::path) t
  in
  tcps [] test

(* Collects the test statistics *)
type counts = {cases : int; tried : int; errors : int; failures : int;}

let init_counts n = {cases = n; tried = 0; errors = 0; failures = 0;}

let inc_tried c = {cases = c.cases; tried = c.tried + 1; 
		   errors = c.errors; failures = c.failures}

let inc_errors c = {cases = c.cases; tried = c.tried + 1; 
		    errors = c.errors + 1; failures = c.failures}

let inc_failures c = {cases = c.cases; tried = c.tried + 1; 
		      errors = c.errors; failures = c.failures + 1}

let was_successful c = (c.errors = 0) & (c.failures = 0)

let string_of_counts counts =
  "Cases: " ^ string_of_int counts.cases ^ 
  " Tried: " ^ string_of_int counts.tried ^
  " Errors: " ^ string_of_int counts.errors ^
  " Failures: " ^ string_of_int counts.failures

(* Test events *)
type test_event = 
    EStart of path * counts
  | EEnd of path * counts
  | ESuccess of path * counts
  | EFailure of path * string * counts
  | EError of path * string * counts

let get_counts = function
    EStart(_, c) -> c
  | EEnd(_, c) -> c
  | ESuccess(_, c) -> c
  | EFailure(_, _, c) -> c
  | EError(_, _, c) -> c

(* Run all tests, report starts, errors, failures, and return counts *)
let perform_test report test =
  let report_event event = report event; get_counts event in

  (* this function reports the results of the test *)
  let really_run f path counts =
    try 
      f ();
      report_event (ESuccess (path, inc_tried counts))
    with
      Failure s -> 
	report_event (EFailure(path, s, inc_failures counts))
    | s -> 
	report_event (EError(path, (Printexc.to_string s), inc_errors counts)) 
  in
  (* this function performs all test-cases in the test *)
  let rec pt path counts = function
      TestCase(f) -> 
	report (EStart (path, counts));
	report_event (EEnd (path, really_run f path counts))
    | TestList(tests) -> 
	fold_lefti (fun cn t i -> pt ((ListItem i)::path) cn t) counts tests
    | TestLabel(l, t) -> pt ((Label l)::path) counts t 
  in
  pt [] (init_counts (test_case_count test)) test

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = Unix.gettimeofday () in
  (Unix.gettimeofday () -. begin_time, f x y)

(* A simple (currently too simple) text based test runner *)
let run_test_tt ?(verbose=false) test =
  let printf = Format.printf in
  let separator1 = 
    "======================================================================" in
  let separator2 = 
    "----------------------------------------------------------------------" in
  let errors = ref [] in
  let failures = ref [] in
  let report_event = function
      EStart(p, _) -> if verbose then printf "%s ... @?" (string_of_path p)
    | ESuccess _ -> if verbose then printf "ok@." else printf ".@?"
    | EFailure(path, str, _) ->
	failures := (path, str)::!failures;
	if verbose then printf "FAIL@." else printf "F@?"
    | EError(path, str, _) -> 
	errors := (path, str)::!errors;
	if verbose then printf "ERROR@." else printf "E@?"
    | EEnd _ -> ()  
  in
  let print_error_list flavour = function
      [] -> ()
    | errors -> List.iter 
	  (fun (p, s) -> printf "%s@[%s: %s@\n@\n%s@]@\n%s@." 
	      separator1 flavour (string_of_path p) s separator2) 
	  errors
  in
  
  (* Now start the test *)
  let running_time, counts = time_fun perform_test report_event test in
 
  (* And print the final report *)
  if verbose = false then printf "@.";
  
  print_error_list "ERROR" (List.rev !errors);
  print_error_list "FAIL" (List.rev !failures);

  printf "Ran: %d tests in: %.2f Seconds@." counts.tried running_time;
  
  if was_successful counts then
    printf "OK@."
  else
    printf "FAILED: %s@." (string_of_counts counts);

  counts

(* Call this one from you test suites *)
let run_test_tt_main suite = 
  let verbose = ref false in 
  let set_verbose _ = verbose := true in 
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  let result = (run_test_tt ~verbose:!verbose suite) in
  if not (was_successful result) then
    exit 1;
  result
  
      
    
  
  
  
  






