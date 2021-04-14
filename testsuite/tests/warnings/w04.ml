(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

[@@@ocaml.warning "+4"]

type expr = E of int [@@unboxed]


let f x = match x with (E e) -> e

type t = A | B

let g x = match x with
| A -> 0
| _ -> 1
