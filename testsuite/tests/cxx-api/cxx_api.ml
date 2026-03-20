(* TEST
 has-cxx;
 hassysthreads;
 readonly_files = "stubs.cpp cxx.sh";
 include runtime_events;
 include systhreads;
 {
   setup-ocamlopt.byte-build-env;
   script = "${cxx} ${cppflags} ${cflags} \
     -I ${ocamlsrcdir}/runtime \
     -I ${ocamlsrcdir}/otherlibs/systhreads \
     -I ${ocamlsrcdir}/otherlibs/unix \
     ${outputobj}${test_build_directory}/stubs.${objext} \
     -c ${test_source_directory}/stubs.cpp";
   script;
   all_modules = "stubs.${objext} cxx_api.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
 {
   setup-ocamlc.byte-build-env;
   script = "${cxx} ${cppflags} ${cflags} \
     -I ${ocamlsrcdir}/runtime \
     -I ${ocamlsrcdir}/otherlibs/systhreads \
     -I ${ocamlsrcdir}/otherlibs/unix \
     ${outputobj}${test_build_directory}/stubs.${objext} \
     -c ${test_source_directory}/stubs.cpp";
   script;
   all_modules = "stubs.${objext} cxx_api.ml";
   flags = "-output-complete-exe  -cclib -lunixbyt -cclib -lthreads -cclib -lcamlruntime_eventsbyt";
   ocamlc.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
*)

external test_cxx : unit -> string = "test_cxx"

let () = print_string (test_cxx ())
