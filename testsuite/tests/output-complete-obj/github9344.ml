(* TEST
 use_runtime = "false";
 setup-ocamlc.byte-build-env;
 flags = "-w -a -output-complete-exe -ccopt -I${ocamlsrcdir}/runtime";
 program = "github9344";
 ocamlc.byte;
 program = "sh ${test_source_directory}/github9344.sh";
 run;
 check-program-output;
*)

raise Not_found
