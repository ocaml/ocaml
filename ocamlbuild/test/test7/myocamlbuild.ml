open Ocamlbuild_plugin;;
use_lib "main" "bbcc";;
dep ["ocaml"; "link"; "byte"; "my_cool_plugin"] ["cool_plugin.cmo"];;
