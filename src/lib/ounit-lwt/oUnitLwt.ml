(** Helper to write Lwt tests with OUnit.

    As of 2019-09-19, this module is still experimental.
 *)

let () = OUnitRunnerProcesses.unix_fork := Lwt_unix.fork

(** [lwt_wrapper f] transforms an Lwt function into a test.

Example:
{[
let test =
  "SimpleAssertion" >::
  (lwt_wrapper
     (fun ctxt ->
        Lwt.return 4
        >>= fun i ->
        Lwt.return (assert_equal ~ctxt 4 i)))
]}
  *)
let lwt_wrapper f = fun ctxt -> f ctxt |> Lwt_main.run
