
open OUnitTypes

(* Run all tests, sequential version *)
let run_all_tests logger chooser test_cases =
  let run_test_case f path =
    let result =
      try
        f ();
        RSuccess
      with e ->
        let backtrace = 
          if Printexc.backtrace_status () then
            Some (Printexc.get_backtrace ())
          else
            None
        in
          match e with
            | Failure s -> RFailure (s, backtrace)
            | Skip s -> RSkip s
            | Todo s -> RTodo s 
            | s -> RError (Printexc.to_string s, backtrace)
    in
    let position = OUnitLogger.position logger in
      result, position
  in
  let runner (path, f) =
    let result, position =
      OUnitLogger.report logger (TestEvent (path, EStart));
      run_test_case f path
    in
      OUnitLogger.report logger (TestEvent (path, EResult result));
      OUnitLogger.report logger (TestEvent (path, EEnd));
      path, result, position
  in
  let rec iter state =
    match state.tests_planned with 
      | [] ->
          state.results
      | _ ->
          let (path, f) = chooser state in
          let result = runner (path, f) in
            iter
              {
                results = result :: state.results;
                tests_planned =
                  List.filter
                    (fun (path', _) -> path <> path')
                    state.tests_planned
              }
  in
  iter {results = []; tests_planned = test_cases}
