(* TEST
 * hasunix
 * setup-ocamlopt.opt-build-env
 ** ocamlopt.opt
    program2 = "test3.${objext}"
    flags = "-output-obj"
 *** run
     program = "nm test3.o"
     output = "${test_build_directory}/program-output"
     stdout = "${output}"
 **** check-program-output
*)

let () =
  print_endline "Hello world"
