(* TEST
  * setup-ocamlc.byte-build-env
  ** script
   script = "mkdir script"
  *** script
   script = "cp ${test_source_directory}/pr6081.ml script/script.ml"
  **** script
   script = "cp ${test_source_directory}/pr7873_hello.ml script/lib.ml"
  ***** script
   script = "cp ${test_source_directory}/pr6081_hack.ml lib.ml"
  ****** ocamlc.byte
   module = "lib.ml"
  ******* ocamlc.byte
   module = "script/lib.ml"
  ******** script
   script = "sh ${test_source_directory}/pr6081.sh"
  ********* check-program-output
   reference = "${test_source_directory}/pr7873a.reference"
*)
#load "lib.cmo";;
print_int Lib.hello
