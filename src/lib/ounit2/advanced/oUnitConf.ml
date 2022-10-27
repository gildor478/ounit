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

exception Parse_error of string

type conf = OUnitPropList.t

type 'a var = conf -> 'a

type metadata =
    {
      help: string;
      get_print: conf -> string;
      parse_set: string -> conf -> unit;
      cli: conf -> (string * Arg.spec * string) list;
    }

let metaconf = Hashtbl.create 13

let check_variable_name str =
  let () =
    if String.length str = 0 then
      failwith "'' is not a valid name."
  in
  let () =
    match str.[0] with
      | '0' .. '9' | '_' ->
          failwithf
            "%S is not a valid variable name. It must not start with %C."
            str str.[0]
      | _ ->
          ()
  in
    String.iter
      (function
         | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' ->
             ()
         | c ->
             failwithf
               "%S is not a valid variable name. It must not contain %C."
               str c)
      str

let cli_name name =
  let replace_underscores str =
    let b = Buffer.create (String.length str) in
      String.iter
        (function
          | '_' -> Buffer.add_char b '-'
          | c -> Buffer.add_char b c)
        str;
      Buffer.contents b
  in
  "-" ^ replace_underscores name

let subst conf extra_subst str =
  let substitutions = Hashtbl.create (Hashtbl.length metaconf) in
  let () =
    (* Fill the substitutions table. *)
    Hashtbl.iter
      (fun name metadata ->
         Hashtbl.add substitutions name (metadata.get_print conf))
      metaconf;
    List.iter (fun (k, v) -> Hashtbl.add substitutions k v) extra_subst
  in
  let buff = Buffer.create (String.length str) in
    Buffer.add_substitute buff
      (fun var ->
         try
           Hashtbl.find substitutions var
         with Not_found ->
           failwithf "Unknown substitution variable %S in %S." var str)
      str;
    Buffer.contents buff

let make ~name ~parse ~print ~default ~help ~fcli () =
  let () =
    check_variable_name name;
    if Hashtbl.mem metaconf name then
      failwithf
        "Duplicate definition for configuration variable %S." name
  in
  let set, get = OUnitPropList.new_property default in
  let parse_set str conf = set conf (parse str) in
  let get_print conf = print (get conf) in
    Hashtbl.add metaconf name
      {help = help;
       get_print = get_print;
       parse_set = parse_set;
       cli = (fun conf -> fcli (get conf) (set conf))};
    (get: 'a var)

let make_string name default help =
  make
    ~name
    ~parse:(fun s -> s)
    ~print:(fun s -> s)
    ~default
    ~help
    ~fcli:
    (fun _ set ->
       [cli_name name,
        Arg.String set,
        "str "^help])
    ()

let make_string_subst name default help =
  let get = make_string name default help in
    (fun ?(extra_subst=[]) conf ->
       subst conf extra_subst (get conf))

let make_string_opt name default help =
  make
    ~name
    ~parse:
    (function
       | "none" -> None
       | str -> Some str)
    ~print:
    (function
       | Some x -> x
       | None -> "none")
    ~default
    ~help
    ~fcli:
    (fun _ set ->
       [cli_name name,
        Arg.String (fun str -> set (Some str)),
        "str "^help;

        cli_name ("no_"^name),
        Arg.Unit (fun () -> set None),
        Printf.sprintf " Reset value of %s." name])
    ()

let make_string_subst_opt name default opt =
  let get = make_string_opt name default opt in
    (fun ?(extra_subst=[]) conf ->
       match get conf with
         | Some str -> Some (subst conf extra_subst str)
         | None -> None)

let make_int name default help =
  make
    ~name
    ~parse:
    (fun str ->
       try
         int_of_string str
       with Failure _ ->
         raise
           (Parse_error
              (Printf.sprintf "%S is not an integer." str)))
    ~print:string_of_int
    ~default
    ~help
    ~fcli:
    (fun _ set ->
       [cli_name name,
        Arg.Int set,
        "i "^help])
    ()

let make_float name default help =
  make
    ~name
    ~parse:
    (fun str ->
       try
         float_of_string str
       with Failure _ ->
         raise
           (Parse_error
              (Printf.sprintf "%S is not a float." str)))
    ~print:string_of_float
    ~default
    ~help
    ~fcli:
    (fun _ set ->
       [cli_name name,
        Arg.Float set,
        "f "^help])
    ()

let make_bool name default help =
  make
    ~name
    ~parse:
    (fun str ->
       try
         bool_of_string str
       with Failure _ ->
         raise
           (Parse_error
              (Printf.sprintf "%S is not a boolean (true or false)." str)))
    ~print:string_of_bool
    ~default
    ~help
    ~fcli:
    (fun _ set ->
       [cli_name name,
        Arg.Bool set,
        "{true|false} "^help])
    ()

let make_enum name get_enums default help =
  let parse str =
    let enum_lst = get_enums () in
      if not (List.exists (fun (str', _) -> str = str') enum_lst) then
        raise
          (Parse_error
             (Printf.sprintf
                "%S is not an allowed value for %s."
                str name));
      str
  in
  let get =
    make
      ~name
      ~parse
      ~print:(fun s -> s)
      ~default
      ~help
      ~fcli:
      (fun _ set ->
         [cli_name name,
          Arg.Symbol (List.map fst (get_enums ()), set),
          " "^help])
      ()
  in
    fun conf ->
      try
        get conf, List.assoc (get conf) (get_enums ())
      with Not_found ->
        failwithf
          "Enums list for %s has changed during execution." name

let make_exec name =
  let default =
    let pwd = Sys.getcwd () in
    let bn = Filename.concat pwd name in
      if Sys.file_exists (bn^".native") then
        bn^".native"
      else if Sys.file_exists (bn^".byte") then
        bn^".byte"
      else
        name
  in
    make_string name default (Printf.sprintf "Executable %s." name)

let set ~origin conf name value =
  try
    (Hashtbl.find metaconf name).parse_set value conf
  with
    | Not_found ->
        failwithf
          "Variable %S is not defined in the application.\n%s" name origin
    | Parse_error str ->
        failwith (str ^ "\n" ^ origin)

let file_parse conf fn =
  let parse lineno line =
    let origin =
      Printf.sprintf
        "File \"%s\", line %d."
        fn lineno
    in
    match trim (trim_comment line) with
      | "" ->
          ()
      | str ->
          begin
            let name, value =
              try
                Scanf.sscanf str "%s = %S" (fun name value -> name, value)
              with Scanf.Scan_failure _ ->
                begin
                  try
                    Scanf.sscanf str "%s = %s" (fun name value -> name, value)
                  with Scanf.Scan_failure _ ->
                    failwithf "Unparsable line: %s\n%s" line origin
                end
            in
              set ~origin conf name value
          end
  in
  let chn = open_in fn in
  let lineno = ref 0 in
    try
      while true do
        let line = input_line chn in
          incr lineno;
          parse !lineno line
      done;
      ()
    with
      | End_of_file ->
          close_in chn
      | e ->
          close_in chn;
          raise e

let env_parse conf =
  let parse name =
    let uppercase_name =
      let b = Buffer.create (String.length name) in
      String.iter
        (function
          | 'a' .. 'z' as c ->
            Buffer.add_char b (Char.chr ((Char.code c) - 32))
          | c -> Buffer.add_char b c)
        name;
      Buffer.contents b
    in
    let env_name = "OUNIT_" ^ uppercase_name in
    try
      let value = Sys.getenv env_name in
      (* Check and translate double quoted variable. *)
      let value =
        try
          Scanf.sscanf value "%S" (fun s -> s)
        with Scanf.Scan_failure _ ->
          value
      in
      let origin =
        Printf.sprintf "Environment variable %s=%S." env_name value
      in
        set ~origin conf name value
    with Not_found ->
      ()
  in
    Hashtbl.iter (fun name _ -> parse name) metaconf

let cli_parse ?argv extra_specs conf =
  let specs =
    Hashtbl.fold
      (fun _ metadata lst ->
         let cli_lst =
           match metadata.cli conf with
             | (key, spec, doc) :: tl ->
                 (key, spec, doc ^
                  (Printf.sprintf " (default: %s)"
                     (metadata.get_print conf)))
                 :: tl
             | [] -> []
         in
           cli_lst @ lst)
      metaconf
      []
  in
  let all_specs =
    Arg.align
      ([
        "-conf",
        Arg.String (file_parse conf),
        "fn Read configuration file."
      ]
      @ (List.sort Stdlib.compare specs)
      @ extra_specs)
  in
  let arg_parse =
    match argv with
      | Some arr ->
          Arg.parse_argv ~current:(ref 0) arr
      | None ->
          Arg.parse
  in
    arg_parse
      all_specs
      (fun x -> raise (Arg.Bad ("Unexpected argument: " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " options*")

let default ?(preset=[]) () =
  let conf =  OUnitPropList.create () in
    List.iter
      (fun (name, value) ->
         set ~origin:"Preset by program." conf name value)
      preset;
    conf

(** Load test options from file, environment and command line (in this order).
    Not that [extra_specs] is here for historical reason, better use [make] to
    create command line options.
  *)
let load ?preset ?argv extra_specs =
  let conf = default ?preset () in
    if Sys.file_exists "ounit.conf" then
      file_parse conf "ounit.conf";
    env_parse conf;
    cli_parse ?argv extra_specs conf;
    conf

let dump conf =
  Hashtbl.fold
    (fun name metadata lst ->
       (name, metadata.get_print conf) :: lst)
    metaconf
    []
