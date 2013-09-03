
open OUnitTypes
open OUnitUtils

type test_ctxt =
    {
      logger: OUnitLogger.Test.t;
      conf: OUnitConf.conf;
    }

type test_fun = test_ctxt -> unit

(* The type of tests *)
type test =
  | TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

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


