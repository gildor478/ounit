(** Common utilities to run test.
  *)

open OUnitTypes

let run_one_test logger test_case =
  let (test_path, test_fun) = test_case in
  let () = OUnitLogger.report logger (TestEvent (test_path, EStart)) in
  let result =
    try
      let ctxt = { logger = OUnitLogger.Test.create logger test_path } in
      test_fun ctxt;
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
    OUnitLogger.report logger (TestEvent (test_path, EResult result));
    OUnitLogger.report logger (TestEvent (test_path, EEnd));
    test_path, result, position
