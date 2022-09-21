(* TEST
   reference="${test_source_directory}/ellipses.reference"
   output="ellipses.output"
   readonly_files = "${test_source_directory}/ellipses.input"
   script = "${ocamlrun} ${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${readonly_files} -o ${output}"
  * hasstr
  ** hasunix
  *** native-compiler
  **** shared-libraries
  ***** script with unix,str
  ****** check-program-output
*)
