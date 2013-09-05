
open OUnitUtils
open OUnitBracket
open OUnitTest

let skip_if b msg =
  if b then
    raise (Skip msg)

let todo msg =
  raise (Todo msg)

let assert_failure msg =
  failwith ("OUnit: " ^ msg)

let assert_bool msg b =
  if not b then assert_failure msg

let assert_string str =
  if not (str = "") then assert_failure str

let assert_equal ?ctxt ?(cmp = ( = )) ?printer ?pp_diff ?msg expected actual =
  let get_error_string () =
    let res =
      buff_format_printf
        (fun fmt ->
           Format.pp_open_vbox fmt 0;
           begin
             match msg with
               | Some s ->
                   Format.pp_open_box fmt 0;
                   Format.pp_print_string fmt s;
                   Format.pp_close_box fmt ();
                   Format.pp_print_cut fmt ()
               | None ->
                   ()
           end;

           begin
             match printer with
               | Some p ->
                   Format.fprintf fmt
                     "@[expected: @[%s@]@ but got: @[%s@]@]@,"
                     (p expected)
                     (p actual)

               | None ->
                   Format.fprintf fmt "@[not equal@]@,"
           end;

           begin
             match pp_diff with
               | Some d ->
                   Format.fprintf fmt
                     "@[differences: %a@]@,"
                      d (expected, actual)

               | None ->
                   ()
           end;
           Format.pp_close_box fmt ())
    in
    let len =
      String.length res
    in
      if len > 0 && res.[len - 1] = '\n' then
        String.sub res 0 (len - 1)
      else
        res
  in
  let logf fmt =
    match ctxt with
      | Some ctxt ->
          OUnitLogger.Test.logf ctxt.test_logger `Info fmt
      | None ->
          Printf.ksprintf ignore fmt
  in
    begin
      match msg with
        | Some str ->
            logf "%s" str;
        | _ ->
            ()
    end;
    begin
      match printer with
        | Some p ->
            logf "Expected: %s" (p expected);
            logf "Actual: %s" (p actual)
        | _ ->
            ()
    end;

    if not (cmp expected actual) then
      assert_failure (get_error_string ())

let assert_command
    ?(exit_code=Unix.WEXITED 0)
    ?(sinput=Stream.of_list [])
    ?(foutput=ignore)
    ?(use_stderr=true)
    ?env
    ~ctxt
    prg args =

    OUnitTest.section_ctxt ctxt
      (fun ctxt ->
         let (fn_out, chn_out) = bracket_tmpfile ctxt in
         let cmd_print fmt =
           let () =
             match env with
               | Some e ->
                   begin
                     Format.pp_print_string fmt "env";
                     Array.iter (Format.fprintf fmt "@ %s") e;
                     Format.pp_print_space fmt ()
                   end

               | None ->
                   ()
           in
             Format.pp_print_string fmt prg;
             List.iter (Format.fprintf fmt "@ %s") args
         in

         (* Start the process *)
         let in_write =
           Unix.dup (Unix.descr_of_out_channel chn_out)
         in
         let (out_read, out_write) =
           Unix.pipe ()
         in
         let err =
           if use_stderr then
             in_write
           else
             Unix.stderr
         in
         let args =
           Array.of_list (prg :: args)
         in
         let pid =
           OUnitLogger.Test.raw_printf ctxt.test_logger "%s"
             (buff_format_printf
                (fun fmt ->
                   Format.fprintf fmt "@[Starting command '%t'@]\n" cmd_print));
           Unix.set_close_on_exec out_write;
           match env with
             | Some e ->
                 Unix.create_process_env prg args e out_read in_write err
             | None ->
                 Unix.create_process prg args out_read in_write err
         in
         let () =
           Unix.close out_read;
           Unix.close in_write
         in
         let () =
           (* Dump sinput into the process stdin *)
           let buff = " " in
             Stream.iter
               (fun c ->
                  let _i : int =
                    buff.[0] <- c;
                    Unix.write out_write buff 0 1
                  in
                    ())
               sinput;
             Unix.close out_write
         in
         let _, real_exit_code =
           let rec wait_intr () =
             try
               Unix.waitpid [] pid
             with Unix.Unix_error (Unix.EINTR, _, _) ->
               wait_intr ()
           in
             wait_intr ()
         in
         let exit_code_printer =
           function
             | Unix.WEXITED n ->
                 Printf.sprintf "exit code %d" n
             | Unix.WSTOPPED n ->
                 Printf.sprintf "stopped by signal %d" n
             | Unix.WSIGNALED n ->
                 Printf.sprintf "killed by signal %d" n
         in

           (* Dump process output to stderr *)
           begin
             let chn = open_in fn_out in
             let buff = String.make 4096 'X' in
             let len = ref (-1) in
               while !len <> 0 do
                 len := input chn buff 0 (String.length buff);
                 OUnitLogger.Test.raw_printf
                   ctxt.test_logger "%s" (String.sub buff 0 !len);
               done;
               close_in chn
           end;

           (* Check process status *)
           assert_equal
             ~msg:(buff_format_printf
                     (fun fmt ->
                        Format.fprintf fmt
                          "@[Exit status of command '%t'@]" cmd_print))
             ~printer:exit_code_printer
             exit_code
             real_exit_code;

           begin
             let chn = open_in fn_out in
               try
                 foutput (Stream.of_channel chn)
               with e ->
                 close_in chn;
                 raise e
           end)

let raises f =
  try
    f ();
    None
  with e ->
    Some e

let assert_raises ?msg exn (f: unit -> 'a) =
  let pexn =
    Printexc.to_string
  in
  let get_error_string () =
    let str =
      Format.sprintf
        "expected exception %s, but no exception was raised."
        (pexn exn)
    in
      match msg with
        | None ->
            assert_failure str

        | Some s ->
            assert_failure (s^"\n"^str)
  in
    match raises f with
      | None ->
          assert_failure (get_error_string ())

      | Some e ->
          assert_equal ?msg ~printer:pexn exn e

