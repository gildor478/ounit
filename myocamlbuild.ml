
open Ocamlbuild_plugin;;
open Command;;

rule "src/oUnitLoggerHTMLData.ml"
  ~prod:"src/oUnitLoggerHTMLData.ml"
  ~deps:["src/oUnit.css"; "src/oUnit.js"]
  begin
    fun env build ->
      let ounit_css = Printf.sprintf "%S" (read_file "src/oUnit.css") in
      let ounit_js = Printf.sprintf "%S" (read_file "src/oUnit.js") in
        Echo(
          [
            "let oUnit_css = " ^ ounit_css ^ ";;";
            "let oUnit_js = " ^ ounit_js ^ ";;";
          ],
          "src/oUnitLoggerHTMLData.ml")
  end
;;

(* OASIS_START *)
(* OASIS_STOP *)

Ocamlbuild_plugin.dispatch 
  (function
     | After_rules as e ->
         dep ["doc"; "docdir"; "extension:html"; 
              "ocaml"; "oasis_document_api_ounit"] &
           ["doc/manual.txt"];
         flag ["doc"; "docdir"; "extension:html"; 
               "ocaml"; "oasis_document_api_ounit"] &
           (S[A"-t"; A"OUnit user guide"; 
              A"-intro"; P"doc/manual.txt"; 
              A"-colorize-code"]);
         dispatch_default e
     | e -> 
         dispatch_default e)
;;
