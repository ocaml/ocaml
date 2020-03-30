(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
   * bytecode
     reference = "${test_source_directory}/inline_test.byte.reference"
   * native
     reference = "${test_source_directory}/inline_test.opt.reference"
     compare_programs = "false"
   * native
     ocamlopt_flags = "-O3"
     compiler_directory_suffix = ".O3"
     reference = "${test_source_directory}/inline_test.opt.reference"
     compare_programs = "false"
*)

(* A test for inlined stack backtraces *)

let f x =
  raise (Failure "test") + 1

let g x =
  f x + 1

let h x =
  print_int (g x); print_endline "h"

let i x =
  if h x = () then ()

let () =
  Printexc.record_backtrace true;
  i ()
