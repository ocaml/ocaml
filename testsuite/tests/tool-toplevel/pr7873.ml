(* TEST
   files = "pr7873.sh"
  * setup-ocamlc.byte-build-env
  ** script
   script = "mkdir hello"
  *** script
   script = "cp ${test_source_directory}/pr7873_hello.ml hello/hello.ml"
  **** ocamlc.byte
   module = "hello/hello.ml"
  ***** ocamlc.byte
   module = ""
   program = "${test_build_directory}/mytop.exe"
   include ocamlcommon
   libraries += "ocamlbytecomp ocamltoplevel"
   flags = "-I hello"
   all_modules = "pr7873.ml hello.cmo topstart.cmo"
  ****** script
   script = "sh ./pr7873.sh"
  ******* check-program-output
  ****** script
   script = "sh ./pr7873.sh direct"
  ******* check-program-output
   reference = "${test_source_directory}/pr7873a.reference"
*)

let () = Topdirs.dir_directory "hello"
