
type node = ListItem of int | Label of string

let node1_of_node =
  function
    | OUnitTypes.ListItem i -> ListItem i
    | OUnitTypes.Label s -> Label s

let node_of_node1 =
  function
    | ListItem i -> OUnitTypes.ListItem i
    | Label s -> OUnitTypes.Label s

type path = node list

let path1_of_path pth =
  List.map node1_of_node pth

type test_fun = unit -> unit

type test =
    TestCase of test_fun
  | TestList of test list
  | TestLabel of string * test

let default_context =
  {
    OUnitTypes.logger = OUnitLogger.Test.create OUnitLogger.null_logger [];
  }

let rec test1_of_test =
  function
    | OUnitTypes.TestCase f -> TestCase (fun () -> f default_context)
    | OUnitTypes.TestList lst -> TestList (List.map test1_of_test lst)
    | OUnitTypes.TestLabel (str, tst) -> TestLabel (str, test1_of_test tst)

let rec test_of_test1 =
  function
    | TestCase f -> OUnitTypes.TestCase (fun ctxt -> f ())
    | TestList lst -> OUnitTypes.TestList (List.map test_of_test1 lst)
    | TestLabel (str, tst) ->  OUnitTypes.TestLabel (str, test_of_test1 tst)

type test_result =
    RSuccess of path
  | RFailure of path * string
  | RError of path * string
  | RSkip of path * string
  | RTodo of path * string

let test_result1_of_test_result path rslt =
  let path1 =
    path1_of_path path
  in
  let rslt1 =
    match rslt with
     | OUnitTypes.RSuccess ->
         RSuccess path1
     | OUnitTypes.RFailure (str, _) ->
         RFailure (path1, str)
     | OUnitTypes.RError (str, _) ->
         RError (path1, str)
     | OUnitTypes.RSkip str ->
         RSkip (path1, str)
     | OUnitTypes.RTodo str ->
         RTodo (path1, str)
  in
    rslt1


type test_event =
    EStart of path
  | EEnd of path
  | EResult of test_result

let result_path =
  function
    | RSuccess path
    | RError (path, _)
    | RFailure (path, _)
    | RSkip (path, _)
    | RTodo (path, _) -> path

type test_results = test_result list

let list_result1_of_list_result =
  List.map
    (fun (pth, rslt, _) ->
       test_result1_of_test_result pth rslt)

let assert_failure =
  OUnitAssert.assert_failure

let assert_bool =
  OUnitAssert.assert_bool

let ( @? ) =
  OUnitAssert.assert_bool

let assert_string =
  OUnitAssert.assert_string

let assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ?(verbose=false) prg args =
  let ctxt =
    if verbose then
      {
        OUnitTypes.logger = OUnitLogger.Test.create
                              (OUnitLogger.std_logger verbose)
                              []
      }
    else
      default_context
  in
    OUnitAssert.assert_command
      ?exit_code ?sinput ?foutput ?use_stderr ?env ~ctxt
      prg args

let assert_equal ?cmp ?printer ?pp_diff ?msg a b =
  OUnitAssert.assert_equal ?cmp ?printer ?pp_diff ?msg a b

let assert_raises ?msg exc f =
  OUnitAssert.assert_raises ?msg exc f

let skip_if =
  OUnitAssert.skip_if

let todo =
  OUnitAssert.todo

let cmp_float ?epsilon f1 f2 =
  OUnitUtils.cmp_float ?epsilon f1 f2

let bracket pre f post () =
  OUnitBracket.bracket
    (fun _ -> pre ())
    (fun (_, fixture) -> f fixture)
    (fun (_, fixture) -> post fixture)
    default_context

let bracket_tmpfile ?prefix  ?suffix ?mode gen () =
  OUnitBracket.bracket_tmpfile ?prefix  ?suffix ?mode
    (fun (_, fixture) -> gen fixture)
    default_context

let (>:) a b =
  test1_of_test (OUnitTest.(>:) a (test_of_test1 b))

let (>::) a b =
  test1_of_test (OUnitTest.(>::) a (fun _ -> b ()))

let (>:::) a b =
  test1_of_test (OUnitTest.(>:::) a (List.map test_of_test1 b))

let test_decorate g tst =
  test1_of_test
    (OUnitTest.test_decorate
       (fun f ->
          let f1 = (fun () -> f default_context) in
          let f1' = g f1 in
            (fun _ -> f1' ()))
       (test_of_test1 tst))
let test_filter ?skip lst test =
  let res =
    OUnitTest.test_filter ?skip lst (test_of_test1 test)
  in
    match res with
      | Some tst -> Some (test1_of_test tst)
      | None -> None

let test_case_count tst =
  OUnitTest.test_case_count (test_of_test1 tst)

let string_of_node nd =
  OUnitTest.string_of_node (node_of_node1 nd)

let string_of_path pth =
  OUnitTest.string_of_path (List.map node_of_node1 pth)

let test_case_paths tst =
  let lst =
    OUnitTest.test_case_paths (test_of_test1 tst)
  in
    List.map
      (List.map node1_of_node)
      lst

let perform_test logger1 tst =
  let logger =
    OUnitLogger.fun_logger
      (function
         | {OUnitTypes.event = OUnitTypes.GlobalEvent _} ->
             ()
         | {OUnitTypes.event = OUnitTypes.TestEvent (path, test_event)} ->
             begin
               let path1 =
                 path1_of_path path
               in
                 match test_event with
                   | OUnitTypes.EStart ->
                       logger1 (EStart path1)
                   | OUnitTypes.EEnd ->
                       logger1 (EEnd path1)
                   | OUnitTypes.EResult rslt ->
                       logger1 (EResult (test_result1_of_test_result path rslt))
                   | OUnitTypes.ELog _ | OUnitTypes.ELogRaw _ ->
                       ()
             end)
      ignore
  in
    list_result1_of_list_result
      (OUnitCore.perform_test logger (test_of_test1 tst))

let run_test_tt ?verbose test =
  list_result1_of_list_result
    (OUnitCore.run_test_tt ?verbose (test_of_test1 test))

let run_test_tt_main ?arg_specs ?set_verbose test =
  let lst_rslt = ref [] in
  let fexit lst =
    lst_rslt := list_result1_of_list_result lst
  in
    OUnitCore.run_test_tt_main
      ?arg_specs ?set_verbose ~fexit (test_of_test1 test);
    !lst_rslt

