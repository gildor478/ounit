(**
    Heuristic to pick a test to run.

    @author Sylvain Le Gall
  *)

open OUnitTest

type chooser =
    OUnitTest.logger -> path list -> OUnitTest.result_list -> path option

(** Most simple heuristic, just pick the first test. *)
let simple _ tests_planned _ =
  match tests_planned with
    | hd :: _ -> Some hd
    | [] -> None

module Plugin =
  OUnitPlugin.Make
    (struct
       type t = chooser
       let name = "chooser"
       let conf_help =
         "Select the method to choose tests to run."
       let default = simple
     end)

include Plugin

let () =
  register "simple" 0 simple

