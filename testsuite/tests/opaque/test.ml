(* TEST

subdirectories = "fst intf snd"

compile_only = "true"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-I intf -opaque"
all_modules = "intf/opaque_intf.mli"
*** ocamlopt.byte
flags = "-I intf"
all_modules = "intf/opaque_impl.mli intf/regular.mli"
**** copy
src = "intf/opaque_intf.cmi intf/opaque_impl.cmi intf/regular.cmi \
       intf/opaque_intf.mli intf/opaque_impl.mli intf/regular.mli"
dst = "fst/"
***** copy
src = "intf/opaque_intf.cmi intf/opaque_impl.cmi intf/regular.cmi \
       intf/opaque_intf.mli intf/opaque_impl.mli intf/regular.mli"
dst = "snd/"
****** ocamlopt.byte
flags = "-I fst -opaque"
all_modules = "fst/opaque_impl.ml"
******* ocamlopt.byte
flags = "-I snd -opaque"
all_modules = "snd/opaque_impl.ml"
******** ocamlopt.byte
flags = "-I fst"
all_modules = "fst/opaque_intf.ml fst/regular.ml"
********* ocamlopt.byte
flags = "-I snd"
all_modules = "snd/opaque_intf.ml snd/regular.ml"
********** ocamlopt.byte
flags = "-I fst"
all_modules = "test.ml"

(* ordinary compilation *)
*********** ocamlopt.byte
compile_only = "false"
all_modules = "fst/opaque_intf.cmx fst/opaque_impl.cmx fst/regular.cmx test.cmx"
program = "${test_build_directory}/p1.exe"

(* change to opaque interface *)
*********** ocamlopt.byte
compile_only = "false"
all_modules = "snd/opaque_intf.cmx fst/opaque_impl.cmx fst/regular.cmx test.cmx"
program = "${test_build_directory}/p2.exe"

(* change to opaque implementation *)
*********** ocamlopt.byte
compile_only = "false"
all_modules = "fst/opaque_intf.cmx snd/opaque_impl.cmx fst/regular.cmx test.cmx"
program = "${test_build_directory}/p3.exe"

(* change to non-opaque implementation *)
*********** ocamlopt.byte
compile_only = "false"
all_modules = "fst/opaque_intf.cmx fst/opaque_impl.cmx snd/regular.cmx test.cmx"
program = "${test_build_directory}/p4.exe"
ocamlopt_byte_exit_status = "2"

*)

let () =
  print_endline (Opaque_intf.choose "Opaque_intf: First" "Opaque_intf: Second")

let () =
  print_endline (Opaque_impl.choose "Opaque_impl: First" "Opaque_impl: Second")

let () =
  print_endline (Regular.choose "Regular: First" "Regular: Second")
