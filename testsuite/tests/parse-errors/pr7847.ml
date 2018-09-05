(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)

(* https://caml.inria.fr/mantis/view.php?id=7847
   The backquote causes a syntax error; this file should be rejected. *)
external x : unit -> (int,int)`A.t = "x"
