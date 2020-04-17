(* TEST
   reference="${test_source_directory}/redirections.reference"
   output="redirections.output"
   script = "${ocamlrun} ${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${test_source_directory}/${test_file} -o ${output}"
  * hasstr
  ** native-compiler
  *** shared-libraries
  **** script with unix,str
  ***** check-program-output
  *** no-shared-libraries
  **** script with unix,str
   script = "${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${test_source_directory}/${test_file} -o ${output}"
  ***** check-program-output
*)

\begin{caml_example}{toplevel}
[@@@warning "+A"];;
1 + 2. [@@expect error];;
let f x = () [@@expect warning 27];;
\end{caml_example}

\begin{caml_example}{toplevel}
Format.printf "Hello@.";
print_endline "world";;
\end{caml_example}
