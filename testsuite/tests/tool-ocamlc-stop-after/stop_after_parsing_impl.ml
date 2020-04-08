(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
    flags = "-stop-after parsing -dparsetree"
    ocamlc_byte_exit_status = "0"
*** check-ocamlc.byte-output
*)

(* we intentionally write ill-typed output;
   if `-stop-after parsing` was not supported properly,
   the test would fail with an error *)
let _ = (1 + "true") + x
