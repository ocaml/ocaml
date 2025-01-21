(* TEST
 readonly_files = "gcc-9.sh cxx-api.sh filter.jq all-includes.h stubs.cpp";
 include runtime_events;
 include systhreads;
 not-msvc;
 script = "sh ${test_source_directory}/gcc-9.sh ${cc}";
 script;
 {
   setup-ocamlopt.byte-build-env;
   script = "sh ${test_source_directory}/cxx-api.sh ${cc} -xc++ -std=c++11 -I ${ocamlsrcdir}/runtime -I ${ocamlsrcdir}/otherlibs/runtime_events -I ${ocamlsrcdir}/otherlibs/str -I ${ocamlsrcdir}/otherlibs/systhreads -I ${ocamlsrcdir}/otherlibs/unix -o stubs.o -c stubs.cpp";
   script;
   all_modules = "stubs.o all_includes.ml";
   ocamlopt.byte;
   output = "${test_build_directory}/program-output";
   stdout = "${output}";
   run;
   check-program-output;
 }
*)

(*
-ccopt <opt> passes the option <opt> to the C compiler and linker.

We'd want flags = "-ccopt -xc++ -ccopt -std=c++11", but:

On Windows, the FlexDLL linker is called directly by the ocaml
compiler driver, and rightfully rejects -xc++ -std=c++11. On other
systems, the linker is not called directly, but through the (C)
compiler driver. The compiler driver might then interpret anything
that follows these flags, including object files, as C++.

It's currently not possible to pass flags to the C/C++ compiler only
via the OCaml compiler driver, so we build the object file separately.

Some compilers (e.g., Apple clang's 16 and earlier) default to an old
C++ when given the -xc++ flag, even if they default to C11 or newer,
which is why we need -std=c++11 (for atomic support, among other
things).

Restrict the test to GCC >= 9 as it has json reporting, which makes it
easier to filter unavoidable errors. *)

external test_cxx : unit -> string = "test_cxx"

let () = print_string (test_cxx ())
