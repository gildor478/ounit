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

open OUnitTest

type t = (unit -> unit) list

let create set_up tear_down test_ctxt =
  let fixture = set_up test_ctxt in
  let tear_down test_ctxt =
    tear_down fixture test_ctxt
  in
    OUnitShared.Mutex.with_lock
      test_ctxt.shared test_ctxt.tear_down_mutex
      (fun () ->
         test_ctxt.tear_down <- tear_down :: test_ctxt.tear_down);
    fixture

let logf logger lvl fmt = OUnitLogger.Test.logf logger lvl fmt

let bracket_tmpfile ?(prefix="ounit-") ?(suffix=".txt") ?mode test_ctxt =
  create
    (fun test_ctxt ->
       let suffix = "-"^(OUnitTest.get_shard_id test_ctxt)^suffix in
       let (fn, chn) = Filename.open_temp_file ?mode prefix suffix in
       logf test_ctxt.test_logger `Info "Created a temporary file: %S." fn;
       (fn, chn))
    (fun (fn, chn) test_ctxt ->
       (try close_out chn with _ -> ());
       try
         Sys.remove fn;
         logf test_ctxt.test_logger `Info "Removed a temporary file: %S." fn
       with _ ->
         ())
    test_ctxt


let bracket_tmpdir ?(prefix="ounit-") ?(suffix=".dir") test_ctxt =
  let max_attempt = 10 in
  let rec try_hard_mkdir attempt =
    if max_attempt = attempt then begin
      OUnitUtils.failwithf
        "Unable to create temporary directory after %d attempts."
        attempt
    end else begin
      try
        let suffix = "-"^(OUnitTest.get_shard_id test_ctxt)^suffix in
        let tmpdn = Filename.temp_file prefix suffix in
        Sys.remove tmpdn;
        Unix.mkdir tmpdn 0o755;
        tmpdn
      with Unix.Unix_error (Unix.EEXIST, "mkdir", _) ->
        try_hard_mkdir (max_attempt + 1)
    end
  in
  create
    (fun test_ctxt ->
       let tmpdn = try_hard_mkdir 0 in
       logf test_ctxt.test_logger `Info
         "Create a temporary directory: %S." tmpdn;
       tmpdn)
    (fun tmpdn test_ctxt ->
       let log_delete fn =
         logf test_ctxt.test_logger `Info
           "Delete in a temporary directory: %S." fn
       in
       let safe_run f a = try f a with _ -> () in
       let rec rmdir fn =
         Array.iter
           (fun bn ->
              let fn' = Filename.concat fn bn in
              let is_dir =
                try
                  let st = Unix.lstat fn' in
                  st.Unix.st_kind = Unix.S_DIR
                with _ -> false
              in
                if is_dir then begin
                  rmdir fn';
                  safe_run Unix.rmdir fn';
                  log_delete fn'
                end else begin
                  safe_run Sys.remove fn';
                  log_delete fn'
                end)
           (try Sys.readdir fn with _ -> [||])
       in
         rmdir tmpdn;
         safe_run Unix.rmdir tmpdn;
         log_delete tmpdn)
    test_ctxt

let chdir_mutex = OUnitShared.Mutex.create OUnitShared.ScopeProcess

let bracket_chdir dir =
  create
    (fun test_ctxt -> 
       let () =
         OUnitLogger.infof test_ctxt.logger "Change directory to %S" dir;
         try
           OUnitShared.Mutex.lock test_ctxt.shared chdir_mutex;
         with OUnitShared.Lock_failure ->
           failwith "Trying to do a nested chdir."
       in
       let cur_pwd = Sys.getcwd () in
         Unix.chdir dir;
         cur_pwd)
    (fun cur_pwd test_ctxt ->
       Unix.chdir cur_pwd;
       OUnitShared.Mutex.unlock test_ctxt.shared chdir_mutex)

let with_bracket test_ctxt bracket f =
  section_ctxt test_ctxt
    (fun test_ctxt ->
       let res = bracket test_ctxt in
         f res test_ctxt)
