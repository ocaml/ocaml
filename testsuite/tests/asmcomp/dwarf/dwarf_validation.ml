(* TEST
 flags = "-g";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/validate_dwarf_structures.sh dwarf_validation ${ocamlopt} ${arch}";
 script;
*)

(* Test program for DWARF structure validation *)

let rec factorial n =
  if n <= 1 then 1 else n * factorial (n - 1)

let test_function x y =
  let z = x + y in
  factorial z

let () =
  let result = test_function 3 2 in
  Printf.printf "Result: %d\n" result
