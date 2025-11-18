(* TEST
 flags = "-g";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/validate_params.sh param_tracking ${ocamlopt} ${arch}";
 script;
*)

(* Test that function parameters are tracked in DWARF *)

let test_two_params x y =
  x + y

let test_three_params a b c =
  a + b + c

let () =
  let r1 = test_two_params 10 20 in
  let r2 = test_three_params 1 2 3 in
  Printf.printf "Results: %d %d\n" r1 r2
