(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

[@@@ocaml.warning "+4"]

type expr = E of int [@@unboxed]


let f x = match x with (E e) -> e

type t = A | B

let g x = match x with
| A -> 0
| _ -> 1

(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
