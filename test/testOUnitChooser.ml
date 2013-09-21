
open OUnit2
open OUnitChooser
open OUnitTest

module MapString = Map.Make(String)

let choose label = Choose [Label label]
let postpone = ChooseToPostpone
let skip label = ChooseToSkip [Label label]

let check_choices chooser tests expected_choices =
  let result_of_test =
    List.fold_left
      (fun mp (path, _, result) ->
         MapString.add path result mp)
      MapString.empty tests
  in
  let add_result path result t =
    {t with
         tests_passed = (path, result, None) :: t.tests_passed}
  in
  let not_planned path t =
    {t with tests_planned = List.filter ((<>) path) t.tests_planned}
  in
  let rec virtual_run choices t =
    (* Choose with 1+ test still running. *)
    let choice = chooser t in
    (* Finish the running test. *)
    let t =
      match t.tests_running with
        | path :: tl ->
            let result =
              MapString.find (string_of_path path) result_of_test
            in
            let t = add_result path result t in
              {t with tests_running = tl}
        | [] ->
            t
    in
    (* Apply the choice. *)
    let choices = choice :: choices in
      match choice with
        | ChooseToSkip path ->
            virtual_run choices
              (not_planned path (add_result path (RSkip "") t))
        | ChooseToPostpone ->
            virtual_run choices t
        | Choose path ->
            virtual_run choices
              (not_planned path
                 {t with tests_running = path :: t.tests_running})
        | NoChoice ->
            choices, t
  in
  let t =
    {
      tests_planned = List.map (fun (path, _, _) -> [Label path]) tests;
      tests_running = [];
      tests_passed = [];
      cache = List.fold_left
                (fun cache (path, was_result_opt, _) ->
                   match was_result_opt with
                     | Some result ->
                         OUnitCache.add_result [Label path] result cache
                     | None ->
                         cache)
                OUnitCache.default tests
    }
  in
  let actual_choices, t = virtual_run [] t in
    assert_equal
      ~msg:"All tests passed."
      ~printer:string_of_int
      (List.length tests)
      (List.length t.tests_passed);
    assert_equal
      ~msg:"Right choices made."
      ~printer:(fun choices ->
                  String.concat ", " (List.map string_of_choice choices))
      (expected_choices @ [NoChoice])
      (List.rev actual_choices)

let test ?(run=true) ?(failed=false) ?(still=true) label =
  label,
  begin
    if run && failed then
      Some (RFailure ("", None, None))
    else if run then
      Some RSuccess
    else
      None
  end,
  begin
    let now_failed = if still then failed else not failed in
      if now_failed then
        RFailure ("", None, None)
      else
        RSuccess
  end

let tests =
  "Chooser" >:::
  [
    "failfirst" >::
    (fun test_ctxt ->
       check_choices failfirst
         [test "foo"]
         [choose "foo"];
       check_choices failfirst
         [test "foo"; test ~failed:true ~still:false "bar"]
         [choose "bar"; postpone; choose "foo"];
       check_choices failfirst
         [test "foo"; test ~failed:true "bar"]
         [choose "bar"; postpone; skip "foo"])
  ]
