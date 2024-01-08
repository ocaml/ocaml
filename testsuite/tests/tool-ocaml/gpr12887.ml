(* TEST
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   program = "${ocaml}";
   arguments = "gpr12887.cmo";
   exit_status = "2";
   run;
   check-program-output;
*)

let () = failwith "Print me"
