
open OUnit2
open OUnitConf

type t =
    {
      vfloat: OUnitConf.t -> float;
      vint: OUnitConf.t -> int;
      vstring: OUnitConf.t -> string;
    }

let bracket_ounitconf f =
  bracket
    (fun ctxt ->
       (* TODO: we need a lock here. *)
       let pristine_global = OUnitConf.global#dump () in
       pristine_global,
       {
         vfloat =
           make
             ~printer:string_of_float
             "float"
             (fun v -> Arg.Set_float v)
             0.0
             "";
         vint =
           make
             ~printer:string_of_int
             "int"
             (fun v -> Arg.Set_int v)
             0
             "";
         vstring =
           make
             ~printer:(Printf.sprintf "%S")
             "string"
             (fun v -> Arg.Set_string v)
             ""
             "";
       })
    f
    (fun (_, (pristine_global, t)) ->
       OUnitConf.global#load pristine_global;
       (* TODO: release the lock. *)
       ())


let tests =
  "OUnitConf" >:::
  [
    "CLI" >::
    (bracket_ounitconf
       (fun (test_ctxt, (_, t)) ->
          let conf =
            load
              ~argv:[|"foo"; "-float"; "2.0";
                      "-int"; "2"; "-string"; "foo bar"|]
              []
          in
            assert_equal ~printer:string_of_float 2.0 (t.vfloat conf);
            assert_equal ~printer:string_of_int 2 (t.vint conf);
            assert_equal ~printer:(fun s -> s) "foo bar" (t.vstring conf)));

    "File" >::
    (bracket_tmpfile
       (fun (test_ctxt, (fn, chn)) ->
          let () =
            output_string chn
              "float = 1.0;\n\
               int = 1;\n\
               string = \"abcd ef\";";
            close_out chn
          in
            bracket_ounitconf
              (fun (test_ctxt, (_, t)) ->
                 let conf = load ~argv:[|"foo"; "-conf"; fn|] [] in
                   assert_equal ~printer:string_of_float 1.0 (t.vfloat conf);
                   assert_equal ~printer:string_of_int 1 (t.vint conf);
                   assert_equal ~printer:(fun s -> s) "abcd ef"
                     (t.vstring conf))
              test_ctxt))
    (* TODO: test you cannot inject new duplicate values. *)
  ]
