(**************************************************************************)
(* The OUnit library                                                      *)
(*                                                                        *)
(* Copyright (C) 2002-2008 Maas-Maarten Zeeman.                           *)
(* Copyright (C) 2010 OCamlCore SARL                                      *)
(* Copyright (C) 2013 Sylvain Le Gall                                     *)
(*                                                                        *)
(* The package OUnit is copyright by Maas-Maarten Zeeman, OCamlCore SARL  *)
(* and Sylvain Le Gall.                                                   *)
(*                                                                        *)
(* Permission is hereby granted, free of charge, to any person obtaining  *)
(* a copy of this document and the OUnit software ("the Software"), to    *)
(* deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute,           *)
(* sublicense, and/or sell copies of the Software, and to permit persons  *)
(* to whom the Software is furnished to do so, subject to the following   *)
(* conditions:                                                            *)
(*                                                                        *)
(* The above copyright notice and this permission notice shall be         *)
(* included in all copies or substantial portions of the Software.        *)
(*                                                                        *)
(* The Software is provided ``as is'', without warranty of any kind,      *)
(* express or implied, including but not limited to the warranties of     *)
(* merchantability, fitness for a particular purpose and noninfringement. *)
(* In no event shall Maas-Maarten Zeeman be liable for any claim, damages *)
(* or other liability, whether in an action of contract, tort or          *)
(* otherwise, arising from, out of or in connection with the Software or  *)
(* the use or other dealings in the software.                             *)
(*                                                                        *)
(* See LICENSE.txt for details.                                           *)
(**************************************************************************)

open OUnitUtils
open OUnitBracket
open OUnitTest

let skip_if b msg =
  if b then
    raise (Skip msg)

let todo msg =
  raise (Todo msg)

let assert_failure msg =
  raise (OUnit_failure msg)

let assert_bool msg b =
  if not b then assert_failure msg

let assert_string str =
  if not (str = "") then assert_failure str

let rec seq_of_channel channel () =
  match input_char channel with
  | exception End_of_file -> Seq.Nil
  | char -> Seq.Cons (char, seq_of_channel channel)

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
    ?(sinput=Seq.empty)
    ?(foutput=ignore)
    ?(use_stderr=true)
    ?(backtrace=true)
    ?chdir
    ?env
    ~ctxt
    prg args =

  let log_environment_diff () =
    let module SetString = Set.Make(struct
        type t = string
        let compare = String.compare
      end)
    in
    let set_of_array a =
      let ss = ref SetString.empty in
      for i = 0 to (Array.length a) - 1 do
        ss := SetString.add (Array.get a i) !ss
      done;
      !ss
    in
    let current_environment = set_of_array (Unix.environment ()) in
    let initial_environment = set_of_array ctxt.initial_environment in
    if SetString.equal current_environment initial_environment then begin
      OUnitLogger.Test.logf ctxt.test_logger `Info
        "Environment is the same as original environment.";
    end else begin
      OUnitLogger.Test.logf ctxt.test_logger `Info
        "Environment (diff with original environment):";
      SetString.iter
        (fun s -> OUnitLogger.Test.logf ctxt.test_logger `Info "+%s" s)
        (SetString.diff current_environment initial_environment);
      SetString.iter
        (fun s -> OUnitLogger.Test.logf ctxt.test_logger `Info "-%s" s)
        (SetString.diff current_environment initial_environment);
    end
  in
    begin
      match env with
      | Some a when Array.length a = 0 && Sys.os_type = "Win32" ->
          OUnitLogger.Test.logf ctxt.test_logger `Info "%s"
          ("Using an empty environment on Windows could cause "^
           "failure when running command.")
      | _ -> ()
    end;

    OUnitTest.section_ctxt ctxt
      (fun ctxt ->
         let (fn_out, chn_out) = bracket_tmpfile ctxt in
         let cmd_print fmt =
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
         let env =
           let param = "OCAMLRUNPARAM" in
           let analyse_and_fix env =
             let arr = Array.copy env in
             let fixed = ref false in
             let new_var = ref "" in
             for i = 0 to (Array.length arr) - 1 do
               let really_starts, current_value =
                 OUnitUtils.start_substr ~prefix:(param^"=") arr.(i)
               in
                 if really_starts then begin
                   (* Rewrite the params. *)
                   if not (String.contains current_value 'b') then begin
                     arr.(i) <- param^"="^current_value^"b"
                   end;
                   new_var := arr.(i);
                   fixed := true
                 end
             done;
             if !fixed then
               arr
             else
               Array.append arr [|param^"=b"|]
           in
           if backtrace then begin
             (* Analyse of the provided environment. *)
             match env with
               | Some env -> Some (analyse_and_fix env)
               | None -> Some (analyse_and_fix (Unix.environment ()))
           end else begin
             env
           end
         in
         let command_chdir, in_chdir =
           match chdir with
             | Some dn ->
                 dn,
                 fun f ->
                   with_bracket ctxt (bracket_chdir dn)
                     (fun _ _ -> f ())
             | None ->
                 Sys.getcwd (), fun f -> f ()
         in
         let pid =
           OUnitLogger.Test.logf ctxt.test_logger `Info "%s"
             (buff_format_printf
                (fun fmt ->
                   Format.fprintf fmt "Starting command '%t'." cmd_print));
           OUnitLogger.Test.logf ctxt.test_logger `Info "Working directory: %S"
             command_chdir;
           log_environment_diff ();
           Unix.set_close_on_exec out_write;
           match env with
             | Some e ->
                 in_chdir
                   (fun () ->
                     Unix.create_process_env prg args e out_read in_write err)
             | None ->
                 in_chdir
                   (fun () ->
                      Unix.create_process prg args out_read in_write err)
         in
         let () =
           Unix.close out_read;
           Unix.close in_write
         in
         let () =
           (* Dump sinput into the process stdin *)
           let buff = Bytes.make 1 ' ' in
             Seq.iter
               (fun c ->
                  let _i : int =
                    Bytes.set buff 0 c;
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
           (* Dump process output to stderr *)
           begin
             let chn = open_in_bin fn_out in
             let buff = Bytes.make 4096 'X' in
             let len = ref (-1) in
               while !len <> 0 do
                 len := input chn buff 0 (Bytes.length buff);
                 OUnitLogger.Test.raw_printf
                   ctxt.test_logger "%s" Bytes.(to_string (sub buff 0 !len));
               done;
               close_in chn
           end;

           (* Check process status *)
           assert_equal
             ~msg:(buff_format_printf
                     (fun fmt ->
                        Format.fprintf fmt
                          "@[Exit status of command '%t'@]" cmd_print))
             ~printer:string_of_process_status
             exit_code
             real_exit_code;

           begin
             let chn = open_in_bin fn_out in
               try
                 foutput (seq_of_channel chn)
               with e ->
                 close_in chn;
                 raise e
           end)

let raises f =
  try
    let _ = f () in None
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
