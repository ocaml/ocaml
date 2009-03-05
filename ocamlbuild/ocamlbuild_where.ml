let bindir = ref Ocamlbuild_Myocamlbuild_config.bindir;;
let libdir = ref begin
  try Filename.concat (Sys.getenv "OCAMLLIB") "ocamlbuild"
  with Not_found -> Ocamlbuild_Myocamlbuild_config.libdir
end;;
