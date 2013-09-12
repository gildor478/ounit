open OUnitTest
open OUnit2

let perform_test test =
  let null_logger = OUnitLogger.null_logger in
  let conf = OUnitConf.default () in
    OUnitCore.perform_test
      conf
      null_logger
      (snd (OUnitRunner.choice conf))
      (snd (OUnitChooser.choice conf))
      test

let assert_equal_test_result exp res =
  let norm lst =
   let norm_one (path, test_result, pos) =
     let test_result' =
       match test_result with
         | RSuccess -> RSuccess
         | RFailure (str, _, _) -> RFailure (str, None, None)
         | RError (str, _) -> RError(str, None)
         | RSkip str -> RSkip str
         | RTodo str -> RTodo str
     in
       (path, test_result', pos)
   in
     List.sort Pervasives.compare (List.rev_map norm_one lst)
  in
  assert_equal
    ~cmp:
    (fun a b -> norm a = norm b)
    ~printer:
    (fun results ->
      String.concat "; "
        (List.map
           (fun (path, test_result, _) ->
              let spf fmt = Printf.sprintf fmt in
              let string_of_backtrace =
                function
                  | Some str -> spf "Some (%S)" str
                  | None -> "None"
              in
              let test_result_string =
                match test_result with
                  | RSuccess ->
                      "RSuccess"
                  | RFailure (str, pos_opt, backtrace) ->
                      spf "RFailure(%S, _, %s)"
                        str (string_of_backtrace backtrace)
                  | RError (str, backtrace) ->
                      spf "RError(%S, %s)" str (string_of_backtrace backtrace)
                  | RSkip str ->
                      spf "RSkip(%S)" str
                  | RTodo str ->
                      spf "RTodo(%S)" str
              in
                Printf.sprintf "%S, %s"
                  (OUnitTest.string_of_path path) test_result_string)
           (norm results)))
    exp res

