open Ocamlbuild_plugin;;
dispatch begin function
| After_rules ->
    use_lib "main" "bbcc";
    dep ["ocaml"; "link"; "byte"; "my_cool_plugin"] ["cool_plugin.cmo"];
| _ -> ()
end
