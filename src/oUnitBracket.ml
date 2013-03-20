
let bracket set_up f tear_down () =
  let fixture = 
    set_up () 
  in
  let () = 
    try
      let () = f fixture in
        tear_down fixture
    with e -> 
      let () = 
        tear_down fixture
      in
        raise e
  in
    ()

let bracket_tmpfile ?(prefix="ounit-") ?(suffix=".txt") ?mode f =
  bracket
    (fun () ->
       Filename.open_temp_file ?mode prefix suffix)
    f 
    (fun (fn, chn) ->
       begin
         try 
           close_out chn
         with _ ->
           ()
       end;
       begin
         try
           Sys.remove fn
         with _ ->
           ()
       end)


let bracket_tmpdir ?(prefix="ounit-") ?(suffix=".dir") f =
  bracket 
    (fun () ->
       let tmpdn = Filename.temp_file prefix suffix in
         Sys.remove tmpdn;
         Unix.mkdir tmpdn 0o755;
         tmpdn)
    f
    (fun tmpdn ->
       let rec rmdir fn = 
         Array.iter
           (fun bn ->
              let fn' = Filename.concat fn bn in
                if Sys.is_directory fn' then
                  begin
                    rmdir fn';
                    Unix.rmdir fn'
                  end
                else
                  begin
                    Sys.remove fn'
                  end)
           (Sys.readdir fn)
       in
         rmdir tmpdn;
         Unix.rmdir tmpdn)
