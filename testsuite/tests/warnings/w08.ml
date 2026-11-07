(* TEST_BELOW
*)

let f (Some x) = x

let g = function
  | None -> 1

type t = A | B | C

let h = function
  | A -> 1
  | B -> 2

(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
