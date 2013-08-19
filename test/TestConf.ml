
open OUnit2
open OUnitConf

type t = 
    {
      global: OUnitConf.t;
      vfloat: unit -> float;
      vint: unit -> int;
      vstring: unit -> string;
    }

let bracket_ounitconf f = 
  bracket
    (fun ctxt ->
       let t = default () in
         {
           global = t;
           vfloat = make ~t ~printer:string_of_float "float" (fun v -> Arg.Set_float v) 0.0 "";
           vint = make ~t ~printer:string_of_int "int" (fun v -> Arg.Set_int v) 0 "";
           vstring = make ~t ~printer:(Printf.sprintf "%S") "string" (fun v -> Arg.Set_string v) "" "";
         })
    f
    ignore 


let tests = 
  "OUnitConf" >:::
  [
    "CLI" >::
    (bracket_ounitconf
       (fun (test_ctxt, t) ->
         load ~t:t.global 
           ~argv:[|"foo"; "-float"; "2.0"; "-int"; "2"; "-string"; "foo bar"|]
           [];
         assert_equal ~printer:string_of_float 2.0 (t.vfloat ());
         assert_equal ~printer:string_of_int 2 (t.vint ());
         assert_equal ~printer:(fun s -> s) "foo bar" (t.vstring ())));

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
              (fun (test_ctxt, t) ->
                 load ~t:t.global
                   ~argv:[|"foo"; "-conf"; fn|]
                   [];
                   assert_equal ~printer:string_of_float 1.0 (t.vfloat ());
                   assert_equal ~printer:string_of_int 1 (t.vint ());
                   assert_equal ~printer:(fun s -> s) "abcd ef" (t.vstring ()))
              test_ctxt))
  ]
