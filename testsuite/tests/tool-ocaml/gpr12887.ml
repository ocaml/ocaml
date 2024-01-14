(* TEST
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   flags = "gpr12887.cmo";
   ocaml_exit_status = "2";
   ocamlrunparam = "b=0,v=0x0";
   ocaml;
   check-ocaml-output;
*)

let () = failwith "Print me"
