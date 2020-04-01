(* TEST
   reference="${test_source_directory}/ellipses.reference"
   output="ellipses.output"
   files="${test_source_directory}/ellipses.input"
   script = "${ocamlrun} ${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${files} -o ${output}"
  * hasstr
  ** native-compiler
  *** shared-libraries
  **** script with unix,str
  ***** check-program-output
*)
