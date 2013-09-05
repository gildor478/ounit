
open OUnitUtils

exception Skip of string

exception Todo of string

(** See OUnit.mli. *)
type node = ListItem of int | Label of string

(** See OUnit.mli. *)
type path = node list

(** See OUnit2.mli. *)
type backtrace = string option

(** See OUnit.mli. *)
type result =
  | RSuccess
  | RFailure of string * backtrace
  | RError of string * backtrace
  | RSkip of string
  | RTodo of string

(* See OUnit.mli. *)
type result_full = (path * result * OUnitLogger.position option)

type result_list = result_full list

type ctxt =
    (* TODO: hide this to avoid building a context outside. *)
    {
      conf: OUnitConf.conf;
      logger: (path, result) OUnitLogger.logger;
      path: path;
      test_logger: result OUnitLogger.Test.t;
      mutable tear_down: (ctxt -> unit) list;
      non_fatal: result_full list ref;
    }

type log_event_t = (path, result) OUnitLogger.log_event_t
type logger = (path, result) OUnitLogger.logger

type test_fun = ctxt -> unit

(* The type of tests *)
type test =
  | TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

(** Isolate a function inside a context. All the tear down will run before
    returning.
 *)
let section_ctxt ctxt f =
  let old_tear_down = ctxt.tear_down in
  let clean_exit () =
    List.iter (fun tear_down -> tear_down ctxt) ctxt.tear_down;
    ctxt.tear_down <- old_tear_down
  in
    ctxt.tear_down <- [];
    try
      let res = f ctxt in
        clean_exit ();
        res
    with e ->
      clean_exit ();
      raise e

(** Create a context and run the function. *)
let with_ctxt conf logger non_fatal test_path f =
  let ctxt =
    {
      conf = conf;
      logger = logger;
      path = test_path;
      test_logger = OUnitLogger.Test.create logger test_path;
      tear_down = [];
      non_fatal = non_fatal;
    }
  in
    section_ctxt ctxt f

(** Transform an exception in a result. *)
let result_full_of_exception ctxt e =
  let backtrace () =
    if Printexc.backtrace_status () then
      Some (Printexc.get_backtrace ())
    else
      None
  in
  let result =
    match e with
      | Failure s -> RFailure (s, backtrace ())
      | Skip s -> RSkip s
      | Todo s -> RTodo s
      | s -> RError (Printexc.to_string s, backtrace ())
  in
  let position =
    match result with
      | RSuccess | RSkip _ | RTodo _ ->
          None
      | RFailure _ | RError _ ->
          OUnitLogger.position ctxt.logger
  in
    ctxt.path, result, position

(** Isolate a function inside a context, just as [!section_ctxt] but don't
    propagate a failure, register it for later.
  *)
let non_fatal ctxt f =
  try
    section_ctxt ctxt f
  with e ->
    ctxt.non_fatal := result_full_of_exception ctxt e :: !(ctxt.non_fatal)

(* Some shorthands which allows easy test construction *)
let (>:) s t = TestLabel(s, t)             (* infix *)
let (>::) s f = TestLabel(s, TestCase(f))  (* infix *)
let (>:::) s l = TestLabel(s, TestList(l)) (* infix *)

(* Utility function to manipulate test *)
let rec test_decorate g =
  function
    | TestCase f ->
        TestCase (g f)
    | TestList tst_lst ->
        TestList (List.map (test_decorate g) tst_lst)
    | TestLabel (str, tst) ->
        TestLabel (str, test_decorate g tst)

(* Return the number of available tests *)
let rec test_case_count =
  function
    | TestCase _ -> 1
    | TestLabel (_, t) -> test_case_count t
    | TestList l ->
        List.fold_left
          (fun c t -> c + test_case_count t)
          0 l

let string_of_node =
  function
    | ListItem n ->
        string_of_int n
    | Label s ->
        s

module Path =
struct
  type t = path

  let compare p1 p2 =
    Pervasives.compare p1 p2

  let to_string p =
    String.concat ":" (List.rev_map string_of_node p)
end

module MapPath = Map.Make(Path)

let string_of_path =
  Path.to_string

(* Returns all possible paths in the test. The order is from test case
   to root.
 *)
let test_case_paths test =
  let rec tcps path test =
    match test with
      | TestCase _ ->
          [path]

      | TestList tests ->
          List.concat
            (mapi (fun t i -> tcps ((ListItem i)::path) t) tests)

      | TestLabel (l, t) ->
          tcps ((Label l)::path) t
  in
    tcps [] test

(* Test filtering with their path *)
module SetTestPath = Set.Make(String)

let test_filter ?(skip=false) only test =
  let set_test =
    List.fold_left
      (fun st str -> SetTestPath.add str st)
      SetTestPath.empty
      only
  in
  let rec filter_test path tst =
    if SetTestPath.mem (string_of_path path) set_test then
      begin
        Some tst
      end

    else
      begin
        match tst with
          | TestCase f ->
              begin
                if skip then
                  Some
                    (TestCase
                       (fun ctxt ->
                          raise (Skip "Test disabled")))
                else
                  None
              end

          | TestList tst_lst ->
              begin
                let ntst_lst =
                  fold_lefti
                    (fun ntst_lst tst i ->
                       let nntst_lst =
                         match filter_test ((ListItem i) :: path) tst with
                           | Some tst ->
                               tst :: ntst_lst
                           | None ->
                               ntst_lst
                       in
                         nntst_lst)
                    []
                    tst_lst
                in
                  if not skip && ntst_lst = [] then
                    None
                  else
                    Some (TestList (List.rev ntst_lst))
              end

          | TestLabel (lbl, tst) ->
              begin
                let ntst_opt =
                  filter_test
                    ((Label lbl) :: path)
                    tst
                in
                  match ntst_opt with
                    | Some ntst ->
                        Some (TestLabel (lbl, ntst))
                    | None ->
                        if skip then
                          Some (TestLabel (lbl, tst))
                        else
                          None
              end
      end
  in
    filter_test [] test
