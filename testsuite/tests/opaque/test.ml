(* TEST
 subdirectories = "fst intf snd";
 compile_only = "true";
 setup-ocamlopt.byte-build-env;
 flags = "-I intf -opaque";
 all_modules = "intf/opaque_intf.mli";
 ocamlopt.byte;
 flags = "-I intf";
 all_modules = "intf/opaque_impl.mli intf/regular.mli";
 ocamlopt.byte;
 src = "intf/opaque_intf.cmi intf/opaque_impl.cmi intf/regular.cmi intf/opaque_intf.mli intf/opaque_impl.mli intf/regular.mli";
 dst = "fst/";
 copy;
 src = "intf/opaque_intf.cmi intf/opaque_impl.cmi intf/regular.cmi intf/opaque_intf.mli intf/opaque_impl.mli intf/regular.mli";
 dst = "snd/";
 copy;
 flags = "-I fst -opaque";
 all_modules = "fst/opaque_impl.ml";
 ocamlopt.byte;
 flags = "-I snd -opaque";
 all_modules = "snd/opaque_impl.ml";
 ocamlopt.byte;
 flags = "-I fst";
 all_modules = "fst/opaque_intf.ml fst/regular.ml";
 ocamlopt.byte;
 flags = "-I snd";
 all_modules = "snd/opaque_intf.ml snd/regular.ml";
 ocamlopt.byte;
 flags = "-I fst";
 all_modules = "test.ml";
 ocamlopt.byte;
 { (* ordinary compilation *)
   compile_only = "false";
   all_modules = "fst/opaque_intf.cmx fst/opaque_impl.cmx fst/regular.cmx test.cmx";
   program = "${test_build_directory}/p1.exe";
   ocamlopt.byte;
 }{ (* change to opaque interface *)
   compile_only = "false";
   all_modules = "snd/opaque_intf.cmx fst/opaque_impl.cmx fst/regular.cmx test.cmx";
   program = "${test_build_directory}/p2.exe";
   ocamlopt.byte;
 }{ (* change to opaque implementation *)
   compile_only = "false";
   all_modules = "fst/opaque_intf.cmx snd/opaque_impl.cmx fst/regular.cmx test.cmx";
   program = "${test_build_directory}/p3.exe";
   ocamlopt.byte;
 }{ (* change to non-opaque implementation *)
   compile_only = "false";
   all_modules = "fst/opaque_intf.cmx fst/opaque_impl.cmx snd/regular.cmx test.cmx";
   program = "${test_build_directory}/p4.exe";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
 }
*)

let () =
  print_endline (Opaque_intf.choose "Opaque_intf: First" "Opaque_intf: Second")

let () =
  print_endline (Opaque_impl.choose "Opaque_impl: First" "Opaque_impl: Second")

let () =
  print_endline (Regular.choose "Regular: First" "Regular: Second")
