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

(*
   Logger for information and various OUnit events.
 *)

open OUnitUtils

(* See OUnit.mli. *)
type position =
    {
      filename: string;
      line: int;
    }

(** See OUnit.mli. *)
type log_severity = [`Error | `Warning | `Info]

(** See OUnit.mli. *)
type 'result test_event =
  | EStart
  | EEnd
  | EResult of 'result
  | ELog of log_severity * string
  | ELogRaw of string

type ('path, 'result) result_full = ('path * 'result * position option)

(** Events which occur at the global level. *)
type ('path, 'result) global_event =
  | GConf of string * string (** Dump a configuration options. *)
  | GLog of log_severity * string
  | GStart  (** Start running the tests. *)
  | GEnd    (** Finish running the tests. *)
  | GResults of (float * ('path, 'result) result_full list * int)

type ('path, 'result) log_event_t =
  | GlobalEvent of ('path, 'result) global_event
  | TestEvent of 'path * 'result test_event

type ('path, 'result) log_event =
    {
      shard: string;
      timestamp: float;
      event: ('path, 'result) log_event_t;
    }

type ('path, 'result) logger =
    {
      lshard: string;
      fwrite: ('path, 'result) log_event -> unit;
      fpos: unit -> position option;
      fclose: unit -> unit;
    }

let shard_default = OUnitUtils.shardf 0

let string_of_event ev =
  let spf fmt = Printf.sprintf fmt in
  let string_of_log_severity =
    function
      | `Error   -> "`Error"
      | `Warning -> "`Warning"
      | `Info    -> "`Info"
  in
    match ev with
      | GlobalEvent e ->
          begin
            match e with
              | GConf (k, v) -> spf "GConf (%S, %S)" k v
              | GLog (lvl, s) ->
                  spf "GLog (%s, %S)" (string_of_log_severity lvl) s
              | GStart -> "GStart"
              | GEnd -> "GEnd"
              | GResults _ -> "GResults"
          end
      | TestEvent (_,  e) ->
          begin
            match e with
              | EStart ->
                  "EStart"
              | EEnd ->
                  "EEnd"
              | EResult _ ->
                  "EResult (_)"
              | ELog (lvl, str) ->
                  spf "ELog (%s, %S)" (string_of_log_severity lvl) str
              | ELogRaw str ->
                  spf "ELogRaw %S" str
          end


let null_logger =
  {
    lshard = shard_default;
    fwrite = ignore;
    fpos   = (fun () -> None);
    fclose = ignore;
  }


let fun_logger fwrite fclose =
  {
    lshard = shard_default;
    fwrite = (fun log_ev -> fwrite log_ev);
    fpos   = (fun () -> None);
    fclose = fclose;
  }

let post_logger fpost =
  let data = ref [] in
  let fwrite ev = data := ev :: !data in
  let fclose () = fpost (List.rev !data) in
    {
      lshard = shard_default;
      fwrite = fwrite;
      fpos   = (fun () -> None);
      fclose = fclose;
    }

let set_shard shard logger =
  {logger with lshard = shard}

let report logger ev =
  logger.fwrite
    {
      shard = logger.lshard;
      timestamp = now ();
      event = ev;
    }

let infof logger fmt =
  Printf.ksprintf
    (fun str -> report logger (GlobalEvent (GLog (`Info, str))))
    fmt

let warningf logger fmt =
  Printf.ksprintf
    (fun str -> report logger (GlobalEvent (GLog (`Warning, str))))
    fmt

let errorf logger fmt =
  Printf.ksprintf
    (fun str -> report logger (GlobalEvent (GLog (`Error, str))))
    fmt

let position logger =
  logger.fpos ()

let close logger =
  logger.fclose ()

let combine lst =
  let rec fpos =
    function
      | logger :: tl ->
          begin
            match position logger with
              | Some _ as pos ->
                  pos
              | None ->
                  fpos tl
          end
      | [] ->
          None
  in
  let lshard =
    match lst with hd :: _ -> hd.lshard | [] -> shard_default
  in
    {
      lshard = lshard;
      fwrite =
        (fun log_ev ->
           List.iter
             (fun logger ->
                logger.fwrite log_ev) lst);
      fpos   = (fun () -> fpos lst);
      fclose =
        (fun () ->
           List.iter (fun logger -> close logger) (List.rev lst));
    }

module Test =
struct
  type 'result t = 'result test_event -> unit

  let create logger path =
    fun ev ->
      logger.fwrite
        {
          shard = logger.lshard;
          timestamp = now ();
          event = TestEvent (path, ev)
        }

  let raw_printf t fmt =
    Printf.ksprintf
      (fun s -> t (ELogRaw s))
      fmt

  let logf t lvl fmt =
    Printf.ksprintf
      (fun s -> t (ELog (lvl, s)))
      fmt
end
