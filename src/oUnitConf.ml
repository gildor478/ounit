
open OUnitConf_types
open Lexing

module MapString = Map.Make(String)

type t = 
    {
      mutable cli_options: (Arg.key * Arg.spec * Arg.doc) list;
      mutable file_options: (Arg.spec * Arg.doc) MapString.t;
    }

let default () = 
  {
    cli_options  = [];
    file_options = MapString.empty;
  }

let global = default ()

let make name ?(t=global) ?arg_string ?(alternates=[]) fspec default help =
  let data = ref default in

  let make_one (name, fspec, arg_string, help) = 
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
    let rec help_arg = 
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
        | Arg.Bool _ -> "b"
        | Arg.Symbol (lst, _) -> 
            "{"^(String.concat "|" lst)^"}"
        | Arg.Tuple lst ->
            String.concat " " (List.map help_arg lst)
    in
    let help = 
      (match arg_string with 
         | Some str -> str
         | None -> help_arg spec)
      ^ " " ^ help
    in
      t.cli_options  <- (cli_name, spec, help) :: t.cli_options;
      t.file_options <- MapString.add name (spec, help) t.file_options
  in
    List.iter make_one ((name, fspec, arg_string, help) :: alternates);
    (fun () -> !data)


let file_lookup f =
  if Sys.file_exists "ounit.conf" then
    f "ounit.conf"

let file_parse specs fn =
  let rec parse (pos, var, data) =
    let failwithposf fmt = 
      Printf.ksprintf
        (fun str ->
           let str = 
             Printf.sprintf
               "File %S, line %d, characters %d:\n%s"
               pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
               str
           in
             failwith str)
        fmt
    in
    let rec parse' spec data =
      match spec, data with 
        (* Unit. *)

        | Arg.Unit fspec, Unit ->
            fspec () 

        | Arg.Clear var, Unit ->
            var := false

        | Arg.Set var, Unit ->
            var := true

        | (Arg.Unit _|Arg.Clear _|Arg.Set _), _ ->
            failwithposf "Variable %S doesn't expect a value." var

        (* String. *)

        | (Arg.Rest _|Arg.String _|Arg.Set_string _), Unit ->
            failwithposf "Variable %S expects a string." var 

        | Arg.Rest fspec, d  
        | Arg.String fspec, d ->
            fspec (string_of_data d)

        | Arg.Set_string var, d ->
            var := (string_of_data d)
                     
        (* Float. *)

        | Arg.Set_float var, Float (_, f) ->
            var := f

        | Arg.Float fspec, Float (_, f) ->
            fspec f

        | Arg.Set_float var, Int (_, i) ->
            var := (float_of_int i)

        | Arg.Float fspec, Int (_, i) ->
            fspec (float_of_int i)

        | (Arg.Set_float _|Arg.Float _), _ -> 
            failwithposf "Variable %S expects a float." var

        (* Integer. *)

        | Arg.Set_int var, Int (_, i) ->
            var := i

        | Arg.Int fspec, Int (_, i) ->
            fspec i

        | (Arg.Int _ |Arg.Set_int _), _ ->
            failwithposf "Variable %S expects an integer." var


        (* Bool. *)

        | Arg.Bool fspec, Bool (_, b) ->
            fspec b

        | Arg.Bool _, _ ->
            failwithposf "Variable %S expects a boolean." var

        (* Symbol. *)

        | Arg.Symbol (lst, fspec), d -> 
            let str = string_of_data d in
              if List.mem str lst then
                fspec str
              else 
                failwithposf
                  "Expected (%s) but got %S for variable %S."
                  (String.concat "|" lst) str var

        (* Tuple. *)
        | Arg.Tuple lst, Tuple lst' ->
            let count_non_unit =
              List.fold_left
                (fun i ->
                   function
                     | Arg.Unit _ -> i
                     | _ -> i + 1)
                0
                lst
            in
            if count_non_unit <> List.length lst' then
              failwithposf 
                "Expected %d argument but got only %d for variable %S."
                count_non_unit (List.length lst') var
            else 
              let rec apply_tuple lst_spec lst_data =
                match lst_spec with 
                  | (Arg.Unit fspec) :: tl_spec ->
                      fspec (); apply_tuple tl_spec lst_data
                  | spec :: tl_spec ->
                      begin
                        match lst_data with 
                          | data :: tl_data ->
                              parse' spec data; apply_tuple tl_spec tl_data
                          | [] ->
                              (* Already checked. *)
                              assert false
                      end
                  | [] ->
                      ()
              in
                apply_tuple lst lst'

        | Arg.Tuple lst, d ->
            parse' spec (Tuple [d])

    in
    let spec, _ = 
      try 
        MapString.find var specs 
      with Not_found ->
        failwithposf "Variable %S is not defined in the application." var
    in
      parse' spec data
  in
  let chn = open_in fn in
  let lexbuf = Lexing.from_channel chn in
  let lexbuf = 
    {lexbuf with 
      lex_curr_p = 
        {lexbuf.lex_curr_p with  
             pos_fname = fn}}
  in
    try 
      let lst = 
        OUnitConf_parser.main OUnitConf_lexer.token lexbuf
      in
        List.iter parse lst;
        close_in chn
    with e ->
      begin
        let () = close_in chn in
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
          Printf.eprintf 
            "Parsing error at token %S, file '%s' l%d c%d\n%!"
            tok fn line cnum;
          raise e
      end

let load ?(t=global) ?argv extra_specs = 
  let file_specs = t.file_options in
  let () = file_lookup (file_parse file_specs) in
  let cli_specs =
    Arg.align 
      ([
        "-conf",
        Arg.String (file_parse file_specs),
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
