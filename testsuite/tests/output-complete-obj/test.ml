(* TEST

readonly_files = "test.ml_stub.c"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-w -a -output-complete-obj"
program = "test.ml.bc.${objext}"
*** script
script = "${mkexe} -I${ocamlsrcdir}/runtime -o test.ml_bc_stub.exe \
                   test.ml.bc.${objext} ${nativecc_libs} test.ml_stub.c"
output = "${compiler_output}"
**** run
program = "./test.ml_bc_stub.exe"
stdout = "program-output"
stderr = "program-output"
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags = "-w -a -output-complete-obj"
program = "test.ml.exe.${objext}"
*** script
script = "${mkexe} -I${ocamlsrcdir}/runtime -o test.ml_stub.exe \
                   test.ml.exe.${objext} ${bytecc_libs} test.ml_stub.c"
output = "${compiler_output}"
**** run
program = "./test.ml_stub.exe"
stdout = "program-output"
stderr = "program-output"

*)

let () = Printf.printf "Test!!\n%!"
