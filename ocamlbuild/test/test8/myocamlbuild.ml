open Ocamlbuild_pack;;
open Ocamlbuild_plugin;;
let version = "0.1";;
file_rule "myconfig.ml"
  ~prod:"myconfig.ml"
  ~cache:(fun _ _ -> version)
  begin fun _ oc ->
    Printf.fprintf oc "let version = %S;;\n%!" version
  end;;
