(* TEST
compile_only="true"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags="-g -dno-locations -dsource -dparsetree -dtypedtree -dlambda"
*** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dno-locations.ocamlc.reference"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags="-g -dno-locations -dcmm"
*** no-flambda
**** check-ocamlopt.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dno-locations.ocamlopt.clambda.reference"
*** flambda
**** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dno-locations.ocamlopt.flambda.reference"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags="-g -dlocations -dsource -dparsetree -dtypedtree -dlambda"
*** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dlocations.ocamlc.reference"

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
flags="-g -dlocations -dcmm"
*** no-flambda
**** check-ocamlopt.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dlocations.ocamlopt.clambda.reference"
*** flambda
**** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dlocations.ocamlopt.flambda.reference"
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;
