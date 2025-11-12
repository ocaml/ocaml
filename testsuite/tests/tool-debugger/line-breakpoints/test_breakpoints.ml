(* TEST_BELOW
(* Blank lines added here to preserve locations. *)








*)

(* Test program for line number breakpoints *)
(* Line 14 *)
let factorial n =                        (* Line 15 - function entry point *)
  let rec loop acc n =                   (* Line 16 - nested function *)
    if n <= 1 then acc                   (* Line 17 *)
    else loop (acc * n) (n - 1)          (* Line 18 *)
  in
  loop 1 n                               (* Line 20 *)

let test_simple x =                      (* Line 22 *)
  x + 1                                  (* Line 23 *)

let main () =                            (* Line 22 *)
  Printf.printf "factorial 5 = %d\n" (factorial 5);
  Printf.printf "test_simple 10 = %d\n" (test_simple 10);
  ()

let () = main ()

(* TEST
 flags += " -g ";
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)
