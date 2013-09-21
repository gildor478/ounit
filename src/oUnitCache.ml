
open OUnitTest

type cache = OUnitTest.result MapPath.t

let cache_filename =
  OUnitConf.make_string_subst_opt
    "cache_filename"
    (* TODO: oUnit-$(name).cache *)
    (Some (Filename.concat OUnitUtils.buildir "oUnit-$(suite_name).cache"))
    "Cache file to store previous results."

let default = MapPath.empty

let load conf =
  match cache_filename conf with
    | Some fn ->
        begin
          try
            let chn = open_in fn in
            let cache : cache =
              try
                Marshal.from_channel chn
              with e ->
                default
            in
              close_in chn;
              cache
          with _ ->
            default
        end

    | None ->
        default

let dump conf cache =
  match cache_filename conf with
    | Some fn ->
        begin
          try
            let chn = open_out fn in
              Marshal.to_channel chn cache [];
              close_out chn
          with _ ->
            ()
        end

    | None ->
        ()

let get_result path cache =
  try
    Some (MapPath.find path cache)
  with Not_found ->
    None

let add_result path result cache =
  MapPath.add path result cache
