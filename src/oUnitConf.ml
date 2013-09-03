open Lexing

exception Parse_error of string

type conf = OUnitPropList.t

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
          failwith
            (Printf.sprintf
               "%S is not a valid variable name. \
               It must not start with %C"
               str str.[0]);
      | _ ->
          ()
  in
    String.iter
      (function
         | 'A' .. 'Z' | 'a' .. 'z' | '_' | '0' .. '9' ->
             ()
         | c ->
             failwith
               (Printf.sprintf
                  "%S is not a valid variable name. \
                  It must not contain %C"
                  str c))
      str

let cli_name name =
  let cli_name = "-" ^ name in
  for i = 1 to String.length name do
     match cli_name.[i] with
       | '_' -> cli_name.[i] <- '-'
       | _ -> ()
  done;
  cli_name

(* TODO: move in Utils. *)
let is_blank =
  function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

(* TODO: move in Utils. *)
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

(* TODO: move in Utils. *)
let trim_comment s =
  let buff = Buffer.create (String.length s) in
  let idx = ref 0 in
    while !idx < String.length s && s.[!idx] != '#' do
      Buffer.add_char buff s.[!idx];
      incr idx
    done;
    Buffer.contents buff

let make ~name ~parse ~print ~default ~help ~fcli () =
  let () =
    check_variable_name name;
    if Hashtbl.mem metaconf name then
      failwith
        (Printf.sprintf
           "Duplicate definition for configuration variable %S."
           name)
  in
  let set, get = OUnitPropList.new_property default in
  let parse_set str conf = set conf (parse str) in
  let get_print conf = print (get conf) in
    Hashtbl.add metaconf name
      {help = help;
       get_print = get_print;
       parse_set = parse_set;
       cli = (fun conf -> fcli (get conf) (set conf))};
    get

let make_string name default help =
  make
    ~name
    ~parse:(fun s -> s)
    ~print:(fun s -> s)
    ~default
    ~help
    ~fcli:
    (fun get set ->
       [cli_name name,
        Arg.String set,
        "str "^help])
    ()

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
    (fun get set ->
       [cli_name name,
        Arg.String (fun str -> set (Some str)),
        "str "^help;

        cli_name ("no_"^name),
        Arg.Unit (fun () -> set None),
        Printf.sprintf " Reset value of %s." name])
    ()

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
    (fun get set ->
       [cli_name name,
        Arg.Int set,
        "i "^help])
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
    (fun get set ->
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
      (fun get set ->
         [cli_name name,
          Arg.Symbol (List.map fst (get_enums ()), set),
          " "^help])
      ()
  in
    fun conf ->
      try
        List.assoc (get conf) (get_enums ())
      with Not_found ->
        failwith
          (Printf.sprintf
             "Enums list for %s has changed during execution."
             name)

let set ~origin conf name value =
  try
    (Hashtbl.find metaconf name).parse_set value conf
  with
    | Not_found ->
        failwith
          (Printf.sprintf
             "Variable %S is not defined in the application.\n%s" name origin)
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
                    failwith
                      (Printf.sprintf
                         "Unparseable line: %s\n%s" line origin)
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
    let env_name = "OUNIT_" ^ (String.uppercase name) in
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
      (fun name metadata lst ->
         metadata.cli conf @ lst)
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
      @ (List.sort Pervasives.compare specs)
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
  let conf = OUnitPropList.create () in
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
