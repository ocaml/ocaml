(* TEST_BELOW
(* Blank lines added here to preserve locations. *)






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
  i ()

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 {
   bytecode;
 }{
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }
*)
