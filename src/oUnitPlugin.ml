
(** Standard functions for plugin (register, choose). *)

module type SETTINGS =
sig
  type t
  val name: string
  val conf_help: string
  val default_name: string
  val default_value: t
end

module Make(Settings: SETTINGS) =
struct
  let all = ref [0, (Settings.default_name, Settings.default_value)]

  let register name pref f =
    all := (pref, (name, f)) :: !all

  let of_name s =
    try
      List.assoc s (List.map snd !all)
    with Not_found ->
      OUnitUtils.failwithf "Unable to find %s '%s'." Settings.name s

  let choice =
    OUnitConf.make_enum
      Settings.name
      (fun () -> List.map snd !all)
      Settings.default_name
      Settings.conf_help

  let preset lst =
    let _, (default, _) =
      List.fold_left max (List.hd !all) (List.tl !all)
    in
      (Settings.name, default) :: lst

end
