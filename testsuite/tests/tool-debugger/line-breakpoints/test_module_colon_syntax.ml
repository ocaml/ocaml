(* TEST_BELOW
(* Blank lines added here to preserve locations. *)








*)

(* Test program for Module:line breakpoint syntax *)
(* Line 14 *)
let factorial n =                        (* Line 15 *)
  let rec loop acc n =                   (* Line 16 *)
    if n <= 1 then acc                   (* Line 17 *)
    else loop (acc * n) (n - 1)          (* Line 18 *)
  in
  loop 1 n                               (* Line 20 *)

let () = Printf.printf "Result: %d\n" (factorial 5)

(* TEST
 flags += " -g ";
 debugger_script = "${test_source_directory}/module_colon_input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)
