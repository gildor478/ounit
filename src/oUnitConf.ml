open OUnitConf_types
open OUnitUtils
open Lexing

module MapString = Map.Make(String)

type t =
    {
      mutable cli_options:  (Arg.key * Arg.spec * Arg.doc) list;
      mutable file_options: Arg.spec MapString.t;
      mutable env_options:  (string * Arg.spec) list;
      mutable dump: (unit -> string * string) list;
    }

let default () =
  {
    cli_options  = [];
    file_options = MapString.empty;
    env_options  = [];
    dump = [];
  }

let global = default ()

let make
      name
      ?(t=global)
      ?arg_string
      ?(alternates=[])
      ~printer
      fspec
      default
      help =
  let data = ref default in
  let dump () =
    name, (printer !data)
  in
  let make_one (name, fspec, arg_string, help) =
    let env_name = "OUNIT_" ^ (String.uppercase name) in
    let cli_name =
      let buf = Buffer.create (String.length name) in
      let fst = ref true in
        Buffer.add_char buf '-';
        String.iter
          (function
             | 'A' .. 'Z' | 'a' .. 'z' as c ->
                 fst := false;
                 Buffer.add_char buf c
             | '0' .. '9' | '_' as c ->
                 if !fst then
                   failwith
                     (Printf.sprintf
                        "%s is not a valid variable name. \
                         It must not start with %C"
                        name c);
                 fst := false;
                 Buffer.add_char buf (if c = '_' then '-' else c)
             | c ->
                 failwith
                   (Printf.sprintf
                      "%s is not a valid variable name. \
                      It must not contain %C"
                      name c))
          name;
        Buffer.contents buf
    in
    let spec = fspec data in
    let rec cli_help_arg =
      function
        | Arg.Unit _
        | Arg.Clear _
        | Arg.Set _ -> ""
        | Arg.Rest _ -> "..."
        | Arg.Set_float _
        | Arg.Float _ -> "f"
        | Arg.Set_int _
        | Arg.Int _ -> "i"
        | Arg.Set_string _
        | Arg.String _ -> "s"
        | Arg.Bool _ -> "{true,false}"
        | Arg.Symbol (lst, _) ->
            "{"^(String.concat "|" lst)^"}"
        | Arg.Tuple lst ->
            String.concat " " (List.map cli_help_arg lst)
    in
    let data_help_arg =
      function
        | Arg.Unit _
        | Arg.Clear _
        | Arg.Set _ ->
            cli_help_arg (Arg.Bool ignore)
        | arg ->
            cli_help_arg arg
    in
    let cli_arg_string =
      match arg_string with
        | Some str -> str
        | None -> cli_help_arg spec
    in
    let data_arg_string =
      match arg_string with
        | Some str -> str
        | None -> data_help_arg spec
    in
    let cli_help =
      Printf.sprintf
        "%s %s (file %s=%s; environment %s=%s)"
        cli_arg_string help name data_arg_string env_name data_arg_string
    in
      t.cli_options  <- (cli_name, spec, cli_help) :: t.cli_options;
      t.file_options <- MapString.add name spec t.file_options;
      t.env_options <- (env_name, spec) :: t.env_options

  in
    List.iter make_one ((name, fspec, arg_string, help) :: alternates);
    t.dump <- dump :: t.dump;
    (fun () -> !data)


let file_lookup f =
  if Sys.file_exists "ounit.conf" then
    f "ounit.conf"

let file_parse specs fn =
  let parse (pos, var, data) =
    let failwithposf fmt =
      Printf.ksprintf
        (fun str ->
           let str =
             Printf.sprintf
               "File %S, line %d, characters %d-%d:\n%s"
               pos.pos_fname pos.pos_lnum
               (pos.pos_cnum - pos.pos_bol)
               (pos.pos_cnum - pos.pos_bol)
               str
           in
             failwith str)
        fmt
    in
    let spec =
      try
        MapString.find var specs
      with Not_found ->
        failwithposf "Variable %S is not defined in the application." var
    in
      try
        OUnitConf_data.data_set spec data
      with
        | OUnitConf_data.ExpectNoValue ->
            failwithposf "Variable %S doesn't expect a value." var
        | OUnitConf_data.ExpectType typ ->
            failwithposf "Variable %S expects a %s." var typ
        | OUnitConf_data.ExpectEnum (str, lst) ->
            failwithposf
              "Expected (%s) but got %S for variable %S."
              (String.concat "|" lst) str var
        | OUnitConf_data.ExpectArgCount (count_non_unit, count) ->
            failwithposf
              "Expected %d argument but got only %d for variable %S."
              count_non_unit count var
  in
  let chn = open_in fn in
  let lst =
    try_parse
      (OUnitConf_parser.main OUnitConf_lexer.token)
      (set_pos_fname (Lexing.from_channel chn) fn)
      (fun () -> close_in chn)
  in
    List.iter parse lst

let env_parse env_specs =
  let parse (var, spec) =
    try
      let value =
        Sys.getenv var
      in
      let failwithf fmt =
        Printf.ksprintf
          (fun str ->
             let str =
               Printf.sprintf
                 "Environment variable %s=%S:\n%s"
                 var value str
             in
               failwith str) fmt
      in
        try
          begin
            try
              (* It is hard to know for an environment variable if we are
               * refering as "value" or value, try both.
               *)
              OUnitConf_data.data_set spec (String value)
            with _ ->
              OUnitConf_data.data_set spec
                (OUnitConf_data.data_of_string
                   ~origin:(Printf.sprintf "env %s=%S" var value)
                   value)
          end
        with
          | OUnitConf_data.ExpectNoValue ->
              failwithf "Variable %S doesn't expect a value." var
          | OUnitConf_data.ExpectType typ ->
              failwithf "Variable %S expects a %s." var typ
          | OUnitConf_data.ExpectEnum (str, lst) ->
              failwithf
                "Expected (%s) but got %S for variable %S."
                (String.concat "|" lst) str var
          | OUnitConf_data.ExpectArgCount (count_non_unit, count) ->
              failwithf
                "Expected %d argument but got only %d for variable %S."
                count_non_unit count var
      with Not_found ->
        ()
  in
    List.iter parse env_specs

(** Load test options from file, environment and command line (in this order).
    Not that [extra_specs] is here for historical reason, better use [make] to
    create command line options.
  *)
let load ?(t=global) ?argv extra_specs =
  let () = file_lookup (file_parse t.file_options) in
  let () = env_parse t.env_options in
  let cli_specs =
    Arg.align
      ([
        "-conf",
        Arg.String (file_parse t.file_options),
        "fn Read configuration file."
      ] @ (List.rev t.cli_options) @ extra_specs)
  in
  let anon_fun x = raise (Arg.Bad ("Unexpected argument: " ^ x)) in
  let usage = "usage: " ^ Sys.argv.(0) ^ " options*" in
    match argv with
      | Some arr ->
          Arg.parse_argv ~current:(ref 0) arr cli_specs anon_fun usage
      | None ->
          Arg.parse cli_specs anon_fun usage

let dump ?(t=global) output =
  List.iter
    (fun f ->
       output
         (OUnitTypes.GlobalEvent
            (let k, v = f () in
               OUnitTypes.GConf (k, v))))
    t.dump
