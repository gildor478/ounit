
let make_filename = List.fold_left Filename.concat

let testdata_default =
  let pwd = Sys.getcwd () in
  let is_dir lst =
    let dn = make_filename pwd lst in
      Sys.file_exists dn && Sys.is_directory dn
  in
    try
      let path =
        List.find is_dir
          [
            ["test"; "data"];
            ["tests"; "data"];
            ["data"]
          ]
      in
        Some (make_filename pwd path)
    with Not_found ->
      None

let testdata_dir =
  OUnitConf.make_string_opt
    "testdata_dir"
    testdata_default
    "Location of the test data directory (absolute path)."


let in_testdata_dir conf path =
  match testdata_dir conf with
    | Some fn -> make_filename fn path
    | None ->
        failwith "Test data dir not defined."
