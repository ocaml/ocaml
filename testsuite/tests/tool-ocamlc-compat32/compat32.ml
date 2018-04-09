(* TEST
   * arch64
   ** setup-ocamlc.byte-build-env
   *** ocamlc.byte
       compile_only = "true"
       flags = "-compat-32"
       ocamlc_byte_exit_status = "2"
   **** ocamlc.byte
        ocamlc_byte_exit_status = "0"
        flags = ""
   ***** ocamlc.byte
         compile_only = "false"
         all_modules = "compat32.cmo"
         flags = "-compat-32 -a"
         program = "compat32.cma"
         ocamlc_byte_exit_status = "2"
   ****** ocamlc.byte
          flags = "-a"
          program = "compat32.cma"
          ocamlc_byte_exit_status = "0"
   ******* ocamlc.byte
           all_modules = "compat32.cma"
           flags = "-compat-32 -linkall"
           program = "compat32.byte"
           ocamlc_byte_exit_status = "2"
   ******** check-ocamlc.byte-output
*)

let a = 0xffffffffffff
