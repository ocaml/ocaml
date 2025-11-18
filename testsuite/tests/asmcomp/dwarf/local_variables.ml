(* TEST
 flags = "-g";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/validate_local_vars.sh local_variables ${ocamlopt} ${arch}";
 script;
*)

(* Test that local variable names are preserved in DWARF debug info *)

let compute_sum x y =
  let sum = x + y in
  let doubled = sum * 2 in
  let result = doubled + 10 in
  result

let process_values first second third =
  let temp1 = first * second in
  let temp2 = third + 5 in
  let combined = temp1 + temp2 in
  combined

let () =
  let r1 = compute_sum 10 20 in
  let r2 = process_values 2 3 4 in
  Printf.printf "Results: %d %d\n" r1 r2
