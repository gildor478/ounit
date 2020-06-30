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

(**
   Utilities for OUnit
   @author Sylvain Le Gall
  *)

let is_blank =
  function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let rec trim s =
  let strlen = String.length s in
  if strlen = 0 then
    ""
  else if is_blank s.[0] then
    trim (String.sub s 1 (strlen - 1))
  else if is_blank s.[strlen - 1] then
    trim (String.sub s 0 (strlen - 1))
  else
    s

let trim_comment s =
  let buff = Buffer.create (String.length s) in
  let idx = ref 0 in
    while !idx < String.length s && s.[!idx] != '#' do
      Buffer.add_char buff s.[!idx];
      incr idx
    done;
    Buffer.contents buff

let split_lines s =
  let rev_lst = ref [] in
  let buff = Buffer.create 13 in
  let flush () =
    rev_lst := Buffer.contents buff :: !rev_lst;
    Buffer.clear buff
  in
    if String.length s > 0 then
      begin
        String.iter
          (function
             | '\n' -> flush ()
             | c -> Buffer.add_char buff c)
          s;
        flush ();
        List.rev !rev_lst
      end
    else
      []

let starts_with ~prefix s =
  if String.length s >= String.length prefix then
    String.sub s 0 (String.length prefix) = prefix
  else
    false

let start_substr ~prefix s =
  if starts_with ~prefix s then begin
    let prefix_len = String.length prefix in
      true,  String.sub s prefix_len (String.length s - prefix_len)
  end else begin
    false, s
  end

let extract_backtrace_position str =
  let prefixes =
    [
      "Raised at ";
      "Re-raised at ";
      "Raised by primitive operation at ";
      "Called from ";
    ]
  in

  let rec extract_one_line s prefixes =
    match prefixes with
      | [] -> None
      | prefix :: tl ->
          let really_starts, eol = start_substr ~prefix s in
          if really_starts then begin
            if eol = "unknown location" then
              None
            else
              try
                Scanf.sscanf eol "%_s@\"%s@\", line %d, characters %d-%d"
                  (fun fn line _ _ -> Some (fn, line))
              with Scanf.Scan_failure _ ->
                None
          end else begin
            extract_one_line s tl
          end
  in
    List.map
      (fun s -> extract_one_line s prefixes)
      (split_lines str)

let cmp_float ?(epsilon = 0.00001) a b =
  match classify_float a, classify_float b with
  | FP_infinite, FP_infinite -> a = b
  | FP_infinite, _ | _, FP_infinite | FP_nan, _ | _, FP_nan -> false
  | _, _ ->
    abs_float (a -. b) <= epsilon *. (abs_float a) ||
      abs_float (a -. b) <= epsilon *. (abs_float b)

let buff_format_printf f =
  let buff = Buffer.create 13 in
  let fmt = Format.formatter_of_buffer buff in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents buff

(* Applies function f in turn to each element in list. Function f takes
   one element, and integer indicating its location in the list *)
let mapi f l =
  let rec rmapi cnt l =
    match l with
      | [] ->
          []

      | h :: t ->
          (f h cnt) :: (rmapi (cnt + 1) t)
  in
    rmapi 0 l

let fold_lefti f accu l =
  let rec rfold_lefti cnt accup l =
    match l with
      | [] ->
          accup

      | h::t ->
          rfold_lefti (cnt + 1) (f accup h cnt) t
  in
    rfold_lefti 0 accu l

let now () =
  Unix.gettimeofday ()

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x =
  let begin_time = now () in
  let res = f x in
    (now () -. begin_time, res)

let date_iso8601 ?(tz=true) timestamp =
  let tm = Unix.gmtime timestamp in
  let res =
    Printf.sprintf
      "%04d-%02d-%02dT%02d:%02d:%02d"
      (1900 + tm.Unix.tm_year)
      (1 + tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
  in
    if tz then
      res ^ "+00:00"
    else
      res

let buildir =
  (* Detect a location where we can store semi-temporary data:
     - it must survive a compilation
     - it must be removed with 'make clean'
   *)
  let pwd = Sys.getcwd () in
  let dir_exists fn = Sys.file_exists fn && Sys.is_directory fn in
  let concat, dirname = Filename.concat, Filename.dirname in
    List.find
      dir_exists
      [
        concat pwd "_build";
        concat (dirname pwd) "_build";
        concat (dirname (dirname pwd)) "_build";
        pwd
      ]

let failwithf fmt =
  Printf.ksprintf failwith fmt

let opt f = function Some v -> f v | None -> ()

let fqdn () =
    try
        (Unix.gethostbyname (Unix.gethostname ())).Unix.h_name
    with
        Not_found -> "localhost"

let shardf = Printf.sprintf "%s#%02d" (Unix.gethostname ())

let string_of_process_status =
  function
  | Unix.WEXITED n ->
      Printf.sprintf "Exited with code %d" n
  | Unix.WSIGNALED n ->
      Printf.sprintf "Killed by signal %d" n
  | Unix.WSTOPPED n ->
      Printf.sprintf "Stopped by signal %d" n

let make_counter () =
  let data = Hashtbl.create 13 in
  let all () =
    Hashtbl.fold
      (fun k v lst -> (k, v) :: lst)
      data []
  in
  let incr k =
    let v =
      try
        Hashtbl.find data k
      with Not_found ->
        0
    in
      Hashtbl.replace data k (v + 1)
  in
    all, incr
