open Ocamlbuild_plugin;;
dispatch begin function
| After_rules -> ocaml_lib "b/libb"
| _ -> ()
end
