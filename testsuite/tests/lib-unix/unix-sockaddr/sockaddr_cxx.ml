(* TEST
 not-msvc;
 readonly_files = "sockaddr_cxx_aux.cpp";
 hasunix;
 include unix;
 {
   setup-ocamlopt.byte-build-env;
   script = "${cc} -xc++ -std=c++11 ${cppflags} ${cflags} \
     -I ${ocamlsrcdir}/runtime \
     -I ${ocamlsrcdir}/otherlibs/unix \
     -o ${test_build_directory}/sockaddr_cxx_aux.o \
     -c ${test_source_directory}/sockaddr_cxx_aux.cpp";
   script;
   all_modules = "sockaddr_cxx_aux.o sockaddr_cxx.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
 {
   setup-ocamlc.byte-build-env;
   script = "${cc} -xc++ -std=c++11 ${cppflags} ${cflags} \
     -I ${ocamlsrcdir}/runtime \
     -I ${ocamlsrcdir}/otherlibs/unix \
     -o ${test_build_directory}/sockaddr_cxx_aux.o \
     -c ${test_source_directory}/sockaddr_cxx_aux.cpp";
   script;
   all_modules = "sockaddr_cxx_aux.o sockaddr_cxx.ml";
   flags = "-output-complete-exe -cclib -lunixbyt";
   ocamlc.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
*)
external stubs_old : Unix.sockaddr -> Unix.sockaddr = "stubs_old"
external stubs_new : Unix.sockaddr -> Unix.sockaddr = "stubs_new"

let () =
  let addr = Unix.(ADDR_INET (inet6_addr_any, 0)) in
  let addr' = stubs_old addr in
  let addr'' = stubs_new addr in
  assert (addr = addr');
  assert (addr' = addr'');
