(* TEST
 debugger_script = "${test_source_directory}/machine_input";
 debugger;
 debugger_flags = "-machine-readable";
 shared-libraries;
 setup-ocamlc.byte-build-env;
 flags = " -g ";
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

(* Test command history does not interfere with machine-readable mode *)

let () = print_endline "test"
