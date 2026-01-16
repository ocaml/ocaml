(* TEST
 modules = "stubs.c";
 readonly_files = "all-includes.h";
 not-msvc;
 flags = "-ccopt -x -ccopt c++ -ccopt -std=c++11";
*)

external test_cxx : unit -> (int * int) = "test_cxx"

let () =
  let (x, y) = test_cxx () in
  assert (x = 42);
  assert (y = 1337)
