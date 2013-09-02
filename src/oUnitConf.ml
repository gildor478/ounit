open OUnitConf_types
open Lexing

module MapString = Map.Make(String)

type t = OUnitPropList.t

(* Global storage for options description.

   It contains all the metadata but not their value.
   See [!t] which really contains the data.
 *)
let global =
  object
    val mutable set_default = []
    val mutable cli_options = []
    val mutable file_options = MapString.empty
    val mutable env_options = []
    val mutable dump = []

    method add_default set =
      set_default <- set :: set_default

    method apply_default t =
      List.iter (fun set -> set t) set_default

    method add_cli_options cli_name fspec help =
      cli_options <- (cli_name, fspec, help) :: cli_options

    method get_cli_options t =
      List.split
        (List.rev_map
           (fun (cli_name, fspec, help) ->
              let spec, post = fspec t in
                (cli_name, spec, help), post)
           cli_options)

    method add_file_options var_name fspec =
      file_options <- MapString.add var_name fspec file_options

    method get_file_options t var_name =
      (MapString.find var_name file_options) t

    method add_env_options env_name fspec =
      env_options <- (env_name, fspec) :: env_options

    method get_env_options t =
      List.split
        (List.rev_map
           (fun (env_name, fspec) ->
              let spec, post = fspec t in
                (env_name, spec), post)
           env_options)

    method add_dump name (fdump: t -> string) =
      dump <- (name, fdump) :: dump

    method apply_dump t =
      List.rev_map
        (fun (name, fdump) ->
           name, fdump t)
        dump

    (** Test-only, dump the object. *)
    method dump () =
      set_default,
      cli_options,
      file_options,
      env_options,
      dump

    (** Test-only, reset the object. *)
    method load (old_set_default,
                 old_cli_options,
                 old_file_options,
                 old_env_options,
                 old_dump) =
      set_default <- old_set_default;
      cli_options <- old_cli_options;
      file_options <- old_file_options;
      env_options <- old_env_options;
      dump <- old_dump
  end

let apply_post = List.iter (fun f -> f ())

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

let make_cli_help name arg_string help env_name spec =
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
      | Arg.Symbol (lst, _) -> ""
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
    Printf.sprintf
      "%s %s (file %s=%s; environment %s=%s)"
      cli_arg_string help name data_arg_string env_name data_arg_string

let make
      name
      ?arg_string
      ?(alternates=[])
      ~printer
      fspec
      default
      help =
  let () = check_variable_name name in

  let set, get = OUnitPropList.new_property ~default () in

  let r = ref default in

  let make_one (name, fspec, arg_string, help) =
    let env_name = "OUNIT_" ^ (String.uppercase name) in
    let cli_name =
      let cli_name = "-" ^ name in
      for i = 1 to String.length name do
         match cli_name.[i] with
           | '_' -> cli_name.[i] <- '-'
           | _ -> ()
      done;
      cli_name
    in
    let fspec' t =
      let spec =
        (* TODO: argh !!! (thread problem et al, remove that). *)
        r := get t;
        fspec r
      in
      let post () = set t !r in
        spec, post
    in
    let cli_help =
      make_cli_help name arg_string help env_name (fspec (ref default))
    in
      global#add_cli_options cli_name fspec' cli_help;
      global#add_file_options name fspec';
      global#add_env_options env_name fspec'

  in
    global#add_default (fun t -> set t default);
    global#add_dump name (fun t -> printer (get t));
    List.iter make_one ((name, fspec, arg_string, help) :: alternates);
    get

let make_translate
      name
      ?arg_string
      ?alternates
      ~printer
      ~translate
      fspec
      default
      help =
  let raw_value =
    make
      name
      ?arg_string
      ?alternates
      ~printer
      fspec
      default
      help
  in
    fun conf ->
      translate (raw_value conf)

let set t name value =
  let spec, post =
    try
      global#get_file_options t name
    with Not_found ->
      failwith
        (Printf.sprintf
           "Variable %S is not defined in the application." name)
  in
    OUnitConf_data.data_set spec value;
    post ()

let file_parse t fn =
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
    let spec, post =
      (* TODO: refactor with set. *)
      try
        global#get_file_options t var
      with Not_found ->
        failwithposf "Variable %S is not defined in the application." var
    in
      try
        OUnitConf_data.data_set spec data;
        post ()
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
    OUnitConf_data.try_parse
      (OUnitConf_parser.main OUnitConf_lexer.token)
      (OUnitConf_data.set_pos_fname (Lexing.from_channel chn) fn)
      (fun () -> close_in chn)
  in
    List.iter parse lst

let env_parse t =
  (* TODO: refactor with file_parse. *)
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
  let env_options, post = global#get_env_options t in
    List.iter parse env_options;
    apply_post post

let cli_parse ?argv extra_specs t =
  let specs, post =
    let specs, post = global#get_cli_options t in
    Arg.align
      ([
        "-conf",
        Arg.String (file_parse t),
        "fn Read configuration file."
      ] @ specs @ extra_specs),
    post
  in
  let arg_parse =
    match argv with
      | Some arr ->
          Arg.parse_argv ~current:(ref 0) arr
      | None ->
          Arg.parse
  in
    arg_parse
      specs
      (fun x -> raise (Arg.Bad ("Unexpected argument: " ^ x)))
      ("usage: " ^ Sys.argv.(0) ^ " options*");
    apply_post post

let default ?(preset=[]) () =
  let t = OUnitPropList.create () in
    global#apply_default t;
    (* TODO: apply preset *)
    t

(** Load test options from file, environment and command line (in this order).
    Not that [extra_specs] is here for historical reason, better use [make] to
    create command line options.
  *)
let load ?preset ?argv extra_specs =
  let t = default ?preset () in
    if Sys.file_exists "ounit.conf" then
      file_parse t "ounit.conf";
    env_parse t;
    cli_parse ?argv extra_specs t;
    t

let dump = global#apply_dump
