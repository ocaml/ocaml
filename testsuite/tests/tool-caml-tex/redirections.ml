(* TEST
   reference="${test_source_directory}/redirections.reference"
   output="redirections.output"
   files="${test_source_directory}/redirections.input"
   script = "${ocamlrun} ${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${files} -o ${output}"
  * hasstr
  ** native-compiler
  *** shared-libraries
  **** script with unix,str
  ***** check-program-output
  *** no-shared-libraries
  **** script with unix,str
   script = "${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${files} -o ${output}"
  ***** check-program-output
*)
