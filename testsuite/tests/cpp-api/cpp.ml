(* TEST
   modules = "cpp_stubs1.cpp cpp_stubs2.cpp"
   readonly_files = "all-includes.h"
   flags = "-cc c++ -ccopt -Wno-write-strings"
   * native
*)

external test1 : int -> unit = "test_cpp1"
external test2 : int -> unit = "test_cpp2"

let () =
  test1 1;
  test2 2;
  print_endline "C++ api tests succeeded"
