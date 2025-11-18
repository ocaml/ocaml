(* TEST
 flags = "-g";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/validate_source_names.sh source_param_names ${ocamlopt} ${arch}";
 script;
*)

(* Test that source-level parameter names are preserved in DWARF debug info *)

let add_numbers x y z =
  x + y + z

let compute_product first second third =
  first * second * third

let () =
  let r1 = add_numbers 1 2 3 in
  let r2 = compute_product 2 3 4 in
  Printf.printf "Results: %d %d\n" r1 r2
