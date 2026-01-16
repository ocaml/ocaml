(* TEST
 reference = "${test_source_directory}/redirections.reference";
 output = "redirections.output";
 readonly_files = "${test_source_directory}/redirections.input";
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamltex -repo-root ${ocamlsrcdir} ${readonly_files} -o ${output}";
 hasstr;
 hasunix;
 {
   shared-libraries;
   script with unix, str;
   check-program-output;
 }{
   no-shared-libraries;
   script = "${ocamlsrcdir}/tools/ocamltex -repo-root ${ocamlsrcdir} ${readonly_files} -o ${output}";
   script with unix, str;
   check-program-output;
 }
*)
