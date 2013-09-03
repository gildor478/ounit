
open OUnit2
open OUnitConf

type t =
    {
      vint: OUnitConf.conf -> int;
      vstring: OUnitConf.conf -> string;
    }

let bracket_ounitconf f =
  bracket
    (fun ctxt ->
       (* TODO: we need a lock here. *)
       {
         vint = make_int "int" 0 "";
         vstring = make_string "string" "" "";
       })
    f
    (fun (_, t) ->
       Hashtbl.remove metaconf "int";
       Hashtbl.remove metaconf "string";
       (* TODO: release the lock. *)
       ())


let tests =
  "OUnitConf" >:::
  [
    "CLI" >::
    (bracket_ounitconf
       (fun (test_ctxt, t) ->
          let conf =
            load
              ~argv:[|"foo"; "-int"; "2"; "-string"; "foo bar"|]
              []
          in
            assert_equal ~printer:string_of_int 2 (t.vint conf);
            assert_equal ~printer:(fun s -> s) "foo bar" (t.vstring conf)));

    "File" >::
    (bracket_tmpfile
       (fun (test_ctxt, (fn, chn)) ->
          let () =
            output_string chn
              "int = 1\n\
               string = \"abcd ef\"";
            close_out chn
          in
            bracket_ounitconf
              (fun (test_ctxt, t) ->
                 let conf = load ~argv:[|"foo"; "-conf"; fn|] [] in
                   assert_equal ~printer:string_of_int 1 (t.vint conf);
                   assert_equal ~printer:(fun s -> s) "abcd ef"
                     (t.vstring conf))
              test_ctxt))
    (* TODO: test you cannot inject new duplicate values. *)
  ]
