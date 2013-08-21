
open OUnitConf_types
open OUnitTypes
open OUnitUtils

exception ExpectNoValue
exception ExpectType of string
exception ExpectEnum of string * string list
exception ExpectArgCount of int * int

let data_of_string ~origin str =
  try_parse
    (OUnitConf_parser.data OUnitConf_lexer.token)
    (set_pos_fname (Lexing.from_string str) origin)
    ignore

let rec data_set spec data =
  match spec, data with
    (* Unit. *)

    | Arg.Unit fspec, Unit ->
        fspec ()

    | Arg.Unit fspec, Bool (_, b) ->
        if b then
          fspec ()

    | Arg.Clear var, Unit ->
        var := false

    | Arg.Set var, Unit ->
        var := true

    | Arg.Set var, Bool (_, b)
    | Arg.Clear var, Bool (_, b) ->
        var := b

    | (Arg.Unit _|Arg.Clear _|Arg.Set _), _ ->
        raise ExpectNoValue

    (* String. *)

    | (Arg.Rest _|Arg.String _|Arg.Set_string _), Unit ->
        raise (ExpectType "string")

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
        raise (ExpectType "float")

    (* Integer. *)

    | Arg.Set_int var, Int (_, i) ->
        var := i

    | Arg.Int fspec, Int (_, i) ->
        fspec i

    | (Arg.Int _ |Arg.Set_int _), _ ->
        raise (ExpectType "int")

    (* Bool. *)

    | Arg.Bool fspec, Bool (_, b) ->
        fspec b

    | Arg.Bool _, _ ->
        raise (ExpectType "boolean")

    (* Symbol. *)

    | Arg.Symbol (lst, fspec), d ->
        let str = string_of_data d in
          if List.mem str lst then
            fspec str
          else
            raise (ExpectEnum (str, lst))

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
          raise (ExpectArgCount (count_non_unit, List.length lst'))
        else
          let rec apply_tuple lst_spec lst_data =
            match lst_spec with
              | (Arg.Unit fspec) :: tl_spec ->
                  fspec (); apply_tuple tl_spec lst_data
              | spec :: tl_spec ->
                  begin
                    match lst_data with
                      | data :: tl_data ->
                          data_set spec data; apply_tuple tl_spec tl_data
                      | [] ->
                          (* Already checked. *)
                          assert false
                  end
              | [] ->
                  ()
          in
            apply_tuple lst lst'

    | Arg.Tuple lst, d ->
        data_set spec (Tuple [d])
