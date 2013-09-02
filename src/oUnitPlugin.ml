
(** Standard functions for plugin (register, choose). *)

module type SETTINGS =
sig
  type t
  val name: string
  val conf_help: string
  val default: t
end

module Make(Settings: SETTINGS) =
struct
  let all: (string * Settings.t) list ref = ref ["default", Settings.default]

  let default = ref (0, "default")

  let register name pref f =
    all := (name, f) :: !all;
    default := max !default (pref, name)

  let of_name s =
    try
      List.assoc s !all
    with Not_found ->
      failwith
        (Printf.sprintf "Unable to find %s '%s'." Settings.name s)

  let choice =
    OUnitConf.make_translate
      Settings.name
      ~printer:(fun s -> s)
      ~translate:of_name
      (fun r ->
         Arg.Symbol
           (List.map fst !all, (fun str -> r := str)))
      (snd !default)
      Settings.conf_help

  let default = Settings.default

end
