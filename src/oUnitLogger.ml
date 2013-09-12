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
  | GInfo of string  (* TODO: GInfo -> GLog lvl string *)
  | GStart  (** Start running the tests. *)
  | GEnd    (** Finish running the tests. *)
  | GResults of (float * ('path, 'result) result_full list * int)

type ('path, 'result) log_event_t =
  | GlobalEvent of ('path, 'result) global_event
  | TestEvent of 'path * 'result test_event

type ('path, 'result) log_event =
    {
      timestamp: float;
      event: ('path, 'result) log_event_t;
    }

type ('path, 'result) logger =
    {
      fwrite: ('path, 'result) log_event -> unit;
      fpos:   unit -> position option;
      fclose: unit -> unit;
    }

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
              | GInfo s      -> spf "GInfo %S" s
              | GStart       -> "GStart"
              | GEnd         -> "GEnd"
              | GResults _   -> "GResults"
          end
      | TestEvent (path,  e) ->
          begin
            match e with
              | EStart ->
                  "EStart"
              | EEnd ->
                  "EEnd"
              | EResult result ->
                  "EResult (_)"
              | ELog (lvl, str) ->
                  spf "ELog (%s, %S)" (string_of_log_severity lvl) str
              | ELogRaw str ->
                  spf "ELogRaw %S" str
          end


let null_logger =
  {
    fwrite = ignore;
    fpos   = (fun () -> None);
    fclose = ignore;
  }


let fun_logger fwrite fclose =
  {
    fwrite = (fun log_ev -> fwrite log_ev);
    fpos   = (fun () -> None);
    fclose = fclose;
  }

let post_logger fpost =
  let data = ref [] in
  let fwrite ev = data := ev :: !data in
  let fclose () = fpost (List.rev !data) in
    {
      fwrite = fwrite;
      fpos   = (fun () -> None);
      fclose = fclose;
    }

let report logger ev =
  logger.fwrite
    {
      timestamp = now ();
      event = ev;
    }

let infof logger fmt =
  Printf.ksprintf
    (* TODO: level *)
    (fun str -> report logger (GlobalEvent (GInfo str)))
    fmt

let warningf logger fmt =
  Printf.ksprintf
    (* TODO: level *)
    (fun str -> report logger (GlobalEvent (GInfo str)))
    fmt

let errorf logger fmt =
  Printf.ksprintf
    (* TODO: level *)
    (fun str -> report logger (GlobalEvent (GInfo str)))
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
    {
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

  let create driver path =
    fun ev ->
      driver.fwrite
        {
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
