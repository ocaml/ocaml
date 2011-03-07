let bindir = ref Ocamlbuild_Myocamlbuild_config.bindir;;
let libdir = ref begin
  Filename.concat
    (try Sys.getenv "OCAMLLIB"
     with Not_found -> Ocamlbuild_Myocamlbuild_config.libdir)
    "ocamlbuild"
end;;
