(* TEST

flags = "-w A"
compile_only = "true"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output

* no-flambda
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** check-ocamlopt.byte-output

* flambda
compiler_reference = "${test_source_directory}/w55.flambda.reference"
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** check-ocamlopt.byte-output

*)

let f = (fun x -> x + 1) [@inline never]

let g x = (f [@inlined]) x

let h = ref f

let i x = (!h [@inlined]) x

let j x y = x + y

let h x = (j [@inlined]) x

let a x =
  let b = x + 1 in
  fun y -> y + b

let b x y = (a [@inlined]) x y

let c x = x + 1 [@@inline never]
let d x = (c [@inlined]) x
