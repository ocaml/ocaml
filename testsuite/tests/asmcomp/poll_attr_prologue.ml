(* TEST_BELOW
(* Blank lines added here to preserve locations. *)








*)

let[@poll error] rec c x l =
  match l with
  | [] -> 0
  | _ :: tl -> (c[@tailcall]) (x+1) tl

(* TEST
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)
