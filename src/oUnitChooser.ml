(**
    Heuristic to pick a test to run.

    @author Sylvain Le Gall
  *)

open OUnitState
open OUnitTest

type chooser = OUnitTest.logger -> OUnitState.t -> (path * test_fun)

(** Most simple heuristic, just pick the first test. *)
let simple _ state =
  List.hd state.tests_planned

module Plugin =
  OUnitPlugin.Make
    (struct
       type t = chooser
       let name = "chooser"
       let conf_help =
         "Select a the method to choose tests to run."
       let default = simple
     end)

include Plugin

let () =
  register "simple" 0 simple

