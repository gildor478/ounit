
open OUnitTypes 

let bracket set_up f tear_down ctxt =
  let fixture = 
    set_up ctxt 
  in
  let () = 
    try
      let () = f (ctxt, fixture) in
        tear_down (ctxt, fixture)
    with e -> 
      let () = 
        tear_down (ctxt, fixture)
      in
        raise e
  in
    ()

let bracket_tmpfile ?(prefix="ounit-") ?(suffix=".txt") ?mode f =
  bracket
    (fun ctxt ->
       let (fn, chn) = 
         Filename.open_temp_file ?mode prefix suffix
       in
         OUnitLogger.Test.logf ctxt.logger
           LInfo
           "Created a temporary file: '%s'"
           fn;
         (fn, chn))
    f 
    (fun (ctxt, (fn, chn)) ->
       begin
         try 
           close_out chn
         with _ ->
           ()
       end;
       begin
         try
           Sys.remove fn;
           OUnitLogger.Test.logf ctxt.logger
             LInfo
             "Removed a temporary file: '%s'"
             fn
         with _ ->
           ()
       end)


let bracket_tmpdir ?(prefix="ounit-") ?(suffix=".dir") f =
  bracket 
    (fun ctxt ->
       let tmpdn = Filename.temp_file prefix suffix in
         Sys.remove tmpdn;
         Unix.mkdir tmpdn 0o755;
         OUnitLogger.Test.logf ctxt.logger
           LInfo
           "Create a temporary directory: '%s'"
           tmpdn;
         tmpdn)
    f
    (fun (ctxt, tmpdn) ->
       let log_delete fn = 
         OUnitLogger.Test.logf ctxt.logger
           LInfo
           "Delete in a temporary directory: '%s'"
           fn
       in
       let rec rmdir fn = 
         Array.iter
           (fun bn ->
              let fn' = Filename.concat fn bn in
                if Sys.is_directory fn' then
                  begin
                    rmdir fn';
                    Unix.rmdir fn';
                    log_delete fn'
                  end
                else
                  begin
                    Sys.remove fn';
                    log_delete fn'
                  end)
           (Sys.readdir fn)
       in
         rmdir tmpdn;
         Unix.rmdir tmpdn;
         log_delete tmpdn)
