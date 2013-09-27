
open OUnit2
open OUnitConf

type t =
    {
      vint: OUnitConf.conf -> int;
      vstring: OUnitConf.conf -> string;
    }

let bracket_ounitconf =
  bracket
    (fun ctxt ->
       (* TODO: we need a lock here. *)
       {
         vint = make_int "int" 0 "";
         vstring = make_string "string" "" "";
       })
    (fun _ t ->
       Hashtbl.remove metaconf "int";
       Hashtbl.remove metaconf "string";
       (* TODO: release the lock. *)
       ())


let tests =
  "OUnitConf" >:::
  [
    "CLI" >::
    (fun test_ctxt ->
       let t = bracket_ounitconf test_ctxt in
       let conf =
         load
           ~argv:[|"foo"; "-int"; "2"; "-string"; "foo bar"|]
           []
       in
         assert_equal ~printer:string_of_int 2 (t.vint conf);
         assert_equal ~printer:(fun s -> s) "foo bar" (t.vstring conf));

    "File" >::
    (fun test_ctxt ->
       let fn, chn = bracket_tmpfile test_ctxt in
       let t = bracket_ounitconf test_ctxt in
       let () =
         output_string chn
           "int = 1\n\
            string = \"abcd ef\"";
         close_out chn
       in
       let conf = load ~argv:[|"foo"; "-conf"; fn|] [] in
         assert_equal ~printer:string_of_int 1 (t.vint conf);
         assert_equal ~printer:(fun s -> s) "abcd ef" (t.vstring conf));

    "Substitution" >::
    (fun test_ctxt ->
       let _ = bracket_ounitconf test_ctxt in
       let conf = load ~argv:[|"foo"; "-int"; "10"|] [] in
         assert_equal
           ~printer:(fun s -> s)
           "foo-10"
           (subst conf [] "foo-$int"));

    "NoDoubleInject" >::
    (fun test_ctxt ->
       let _ = bracket_ounitconf test_ctxt in
         try
           let _option: conf -> string  = make_string "string" "" "" in
             assert_failure
               "Should not be able to inject duplicate configuration \
                option 'string'."
         with Failure _ ->
           ());
  ]
