let file_to_string f =
  let chan = open_in_bin f in
  let len = in_channel_length chan in
  let res = Bytes.create len in
  really_input chan res 0 len;
  close_in chan;
  Bytes.to_string res

let _ =

  let css = file_to_string "oUnit.css" in
  let js = file_to_string "oUnit.js" in
  let chan = open_out_bin "oUnitLoggerHTMLData.ml" in
  Printf.fprintf chan
    "let oUnit_css = %S;;
     let oUnit_js = %S;;"
    css js;

  close_out chan
