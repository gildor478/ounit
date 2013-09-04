
(**
  * Utilities for OUnit
  *
  * @author Sylvain Le Gall
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

let cmp_float ?(epsilon = 0.00001) a b =
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
  let open Filename in
    List.find
      dir_exists
      [
        concat pwd "_build";
        concat (basename pwd) "_build";
        concat (basename (basename pwd)) "_build";
        pwd
      ]

let failwithf fmt =
  Printf.ksprintf failwith fmt
