
(**
  * Utilities for OUnit
  *
  * @author Sylvain Le Gall
  *)

open OUnitTypes

let cmp_float ?(epsilon = 0.00001) a b =
  abs_float (a -. b) <= epsilon *. (abs_float a) ||
    abs_float (a -. b) <= epsilon *. (abs_float b)

let is_success =
  function
    | RSuccess -> true
    | RFailure _ | RError _  | RSkip _ | RTodo _ -> false

let is_failure =
  function
    | RFailure _ -> true
    | RSuccess | RError _  | RSkip _ | RTodo _ -> false

let is_error =
  function
    | RError _ -> true
    | RSuccess | RFailure _ | RSkip _ | RTodo _ -> false

let is_skip =
  function
    | RSkip _ -> true
    | RSuccess | RFailure _ | RError _  | RTodo _ -> false

let is_todo =
  function
    | RTodo _ -> true
    | RSuccess | RFailure _ | RError _  | RSkip _ -> false

let result_flavour =
  function
    | RError _ -> "Error"
    | RFailure _ -> "Failure"
    | RSuccess -> "Success"
    | RSkip _ -> "Skip"
    | RTodo _ -> "Todo"


let result_msg =
  function
    | RSuccess -> "Success"
    | RError (msg, _)
    | RFailure (msg, _)
    | RSkip msg
    | RTodo msg -> msg

let string_of_node =
  function
    | ListItem n ->
        string_of_int n
    | Label s ->
        s

(* Return the number of available tests *)
let rec test_case_count =
  function
    | TestCase _ -> 1
    | TestLabel (_, t) -> test_case_count t
    | TestList l ->
        List.fold_left
          (fun c t -> c + test_case_count t)
          0 l

module Path =
struct
  type t = path

  let compare p1 p2 =
    Pervasives.compare p1 p2

  let to_string p =
    String.concat ":" (List.rev_map string_of_node p)
end

module MapPath = Map.Make(Path)

let string_of_path =
  Path.to_string

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

let ocaml_position pos =
  Printf.sprintf
    "File \"%s\", line %d, characters 1-1:"
    pos.filename pos.line

let try_parse f lexbuf g =
  try
    let res = f lexbuf in
      g ();
      res
  with e ->
    begin
      let () = g () in
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let fn = lexbuf.Lexing.lex_curr_p.Lexing.pos_fname in
        (* TODO: ocaml error formatting. *)
        Printf.eprintf
          "Parsing error at token %S, file '%s' l%d c%d\n%!"
          tok fn line cnum;
        raise e
    end

let set_pos_fname lexbuf fn =
  {lexbuf with
    Lexing.lex_curr_p =
      {lexbuf.Lexing.lex_curr_p with
           Lexing.pos_fname = fn}}

let now () =
  Unix.gettimeofday ()

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = now () in
  let res = f x y in
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

let was_successful lst =
  List.for_all
    (fun (_, rslt, _) ->
       match rslt with
         | RSuccess | RSkip _ -> true
         | _ -> false)
    lst
