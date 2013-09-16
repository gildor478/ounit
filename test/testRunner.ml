
open OUnit2

let testFakeRunner =
  conf_make_string
    "testFakeRunner"
    "testFakeRunner"
    "The testFakeRunner executable to run."

type test_results = 
    {
      cases: int;
      tried: int;
      errors: int;
      failures: int;
      skip: int;
      todo: int;
    }

let string_of_test_results test_results =
  Printf.sprintf
    "Cases: %d; Tried: %d; Errors: %d; Failures: %d; Skip: %d; Todo: %d"
    test_results.cases test_results.tried test_results.errors
    test_results.failures test_results.skip test_results.todo

let run_test_fake_runner ctxt runner args = 
  let fn, _ = bracket_tmpfile ctxt in
  let () =
    assert_command 
      ~ctxt
      ~exit_code:(Unix.WEXITED 1)
      (testFakeRunner ctxt)
      ("-output-file" :: fn :: "-runner" :: runner :: args);
  in

  let mk str = 
    let r = ref (-1) in
    let regex = 
      Str.regexp (".* I: "^str^": \\([0-9]+\\)\\.$")
    in
      r,
      fun line ->
        if Str.string_match regex line 0 then
          r := int_of_string (Str.matched_group 1 line)
  in

  let cases, fcases = mk "Cases" in 
  let tried, ftried = mk "Tried" in
  let errors, ferrors = mk "Errors" in
  let failures, ffailures = mk "Failures" in
  let skip, fskip = mk "Skip" in
  let todo, ftodo = mk "Todo" in
    
  let rrunner = ref "" in
  let runner_regex = Str.regexp (".* I: Runner: \\([a-z]+\\)$") in
  let frunner line = 
    if Str.string_match runner_regex line 0 then
      rrunner := Str.matched_group 1 line
  in

  let chn = open_in fn in
  let () =
    try 
      while true do
        let line = input_line chn in
        List.iter
          (fun f -> f line)
          [frunner; fcases; ftried; ferrors; ffailures; fskip; ftodo]
      done;
    with End_of_file ->
      close_in chn
  in
    assert_equal
      ~msg:"runner"
      ~printer:(fun s -> s)
      runner
      !rrunner;
    assert_bool "Cases initialized." (!cases >= 0); 
    assert_bool "Tried initialized." (!tried >= 0); 
    assert_bool "Errors initialized." (!errors >= 0); 
    assert_bool "Failures initialized." (!failures >= 0); 
    assert_bool "Skip initialized." (!skip >= 0); 
    assert_bool "Todo initialized." (!todo >= 0); 
    {
      cases = !cases;
      tried = !tried;
      errors = !errors;
      failures = !failures;
      skip = !skip;
      todo = !todo;
    }

let check_standard_results ?(extra_errors=0) test_results =
  assert_equal
    ~msg:"test results"
    ~printer:string_of_test_results
    {
      cases = 6;
      tried = 6;
      errors = 1 + extra_errors;
      failures = 1;
      skip = 1;
      todo = 1;
    }
    test_results

let skip_if_notunix () = skip_if (Sys.os_type <> "Unix") "Only run on Unix."

let tests = 
  "Runner" >:::
  [
    "Sequential" >::
    (fun ctxt ->
       let test_results = run_test_fake_runner ctxt "sequential" [] in
         check_standard_results test_results);

    "Processes" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" []
       in
         check_standard_results test_results);

    "Processes#1" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" ["-shards"; "1"]
       in
         check_standard_results test_results);

    "Processes#2" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes" ["-shards"; "2"]
       in
         check_standard_results test_results);

    "Processes+SIGSEGV" >::
    (fun ctxt ->
       let test_results =
         skip_if_notunix ();
         run_test_fake_runner ctxt "processes"
           ["-shards"; "2";
            "-sigsegv" ; "true";
            "-health-check-interval"; "0.0"]
       in
         check_standard_results ~extra_errors:1 test_results);

    "Threads" >::
    (fun ctxt ->
       let test_results = run_test_fake_runner ctxt "threads" [] in
         check_standard_results test_results);

    "Threads#1" >::
    (fun ctxt ->
       let test_results =
         run_test_fake_runner ctxt "threads" ["-shards"; "1"]
       in
         check_standard_results test_results);

    "Threads#2" >::
    (fun ctxt ->
       let test_results =
         run_test_fake_runner ctxt "threads" ["-shards"; "2"]
       in
         check_standard_results test_results);
  ]
