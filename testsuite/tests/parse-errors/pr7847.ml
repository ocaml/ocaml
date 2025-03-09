(* TEST_BELOW
(* Blank lines added here to preserve locations. *)



*)

(* https://caml.inria.fr/mantis/view.php?id=7847
   The backquote causes a syntax error; this file should be rejected. *)
external x : unit -> (int,int)`A.t = "x"

(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
