(* TEST
 reference = "${test_source_directory}/history.reference";
 output = "history.output";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamldiaginfo -history -o ${output}";
 script;
 check-program-output;
*)
