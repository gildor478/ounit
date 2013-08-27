(**
    Heuristic to pick a test to run.

    @author Sylvain Le Gall
  *)

open OUnitTypes

type chooser = OUnitLogger.logger -> state -> (path * test_fun)

module Plugin =
  OUnitPlugin.Make
    (struct
       type t = chooser
       let name = "chooser"
       let conf_help =
         "Select a the method to choose tests to run."
     end)

include Plugin

(** Most simple heuristic, just pick the first test. *)
let simple _ state =
  List.hd state.tests_planned

let () =
  register "simple" 0 simple

