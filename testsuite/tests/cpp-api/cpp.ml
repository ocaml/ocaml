(* TEST
   modules = "cpp_stubs1.cpp cpp_stubs2.cpp cpp_stubs3.cpp"
   readonly_files = "all-includes.h"
   flags = "-cc c++"
   * native
*)

external test1 : int -> unit = "test_cpp1"
external test2 : int -> unit = "test_cpp2"
external test3 : int -> unit = "test_cpp3"

let () =
  test1 1;
  test2 2;
  test3 3;
  print_endline "C++ api tests succeeded"
