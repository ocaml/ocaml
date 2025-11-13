(* TEST
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 flags = " -g ";
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

(* Test command history integration does not break basic debugger operation *)

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () =
  print_int (factorial 5);
  print_newline ()
