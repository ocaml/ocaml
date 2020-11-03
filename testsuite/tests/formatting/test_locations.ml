(* TEST
compile_only="true"
* setup-ocamlc.byte-build-env
** ocamlc.byte
flags="-g -dno-unique-ids -dno-locations -dsource -dparsetree -dtypedtree -dlambda"
*** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dno-locations.ocamlc.reference"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags="-g -dno-unique-ids -dlocations -dsource -dparsetree -dtypedtree -dlambda"
*** check-ocamlc.byte-output
compiler_reference =
  "${test_source_directory}/test_locations.dlocations.ocamlc.reference"
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;
