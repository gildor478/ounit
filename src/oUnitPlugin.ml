
(** Standard functions for plugin (register, choose). *)

module type SETTINGS =
sig
  type t
  val name: string
  val conf_help: string
end

module Make(Settings: SETTINGS) =
struct
  let all: (string * Settings.t) list ref = ref []

  let default: (int * string * Settings.t) option ref = ref None

  let register name pref f =
    all := (name, f) :: !all;
    match !default with
      | None ->
          default := Some (pref, name, f)
      | Some (pref', _, _) ->
          if pref > pref' then
            default := Some (pref, name, f)

  let conf () = 

    let default =
      match !default with
        | Some (_, name, _) -> name
        | None ->
            failwith
              (Printf.sprintf "No registered %s plugin." Settings.name)
    in

    let name =
      OUnitConf.make
        Settings.name
        ~printer:(fun s -> s)
        (fun r ->
           Arg.Symbol
             (List.map fst !all, (fun str -> r := str)))
        default
        Settings.conf_help
    in

      (fun () ->
         try
           List.assoc (name ()) !all
         with Not_found ->
           failwith
             (Printf.sprintf
                "Unable to find %s '%s'." Settings.name (name ())))

  let get_default () = 
    match !default with
      | Some (_, _, f) -> f
      | None ->
          failwith
            (Printf.sprintf "No registered %s plugin." Settings.name)
end
