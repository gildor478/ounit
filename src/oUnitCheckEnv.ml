

(* Check environment after and before tests, to check isolation. *)

open OUnitTest
open OUnitAssert

type t =
  {
    pwd: string;
    env: string array;
  }

let create () = 
  {
    pwd = Sys.getcwd ();
    env = Unix.environment ();
  }

module EnvElement =
struct
  type t = string

  let pp_printer = Format.pp_print_string

  let compare = String.compare

  let pp_print_sep = OUnitDiff.pp_comma_separator 
end

module SetEnv = OUnitDiff.SetMake(EnvElement) 

let check test_ctxt t = 
  let t' = create () in
    List.iter 
      (fun f -> non_fatal test_ctxt (fun _ -> f ()))
      [
        (fun () ->
           assert_equal 
             ~msg:"Current working dir (check env)."
             ~printer:(fun s -> s)
             t.pwd
             t'.pwd);
        (fun () ->
           let convert t = SetEnv.of_list (Array.to_list t.env) in
             SetEnv.assert_equal 
               ~msg:"Environment (check env)."
               (convert t)
               (convert t'));
      ]
