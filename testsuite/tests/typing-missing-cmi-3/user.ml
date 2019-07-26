(* TEST

files = "original.ml middle.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "original.ml"
*** ocamlc.byte
module = "middle.ml"
**** script
script = "rm -f original.cmi"
***** ocamlc.byte
module = "user.ml"
*)


let x:'a. 'a Middle.t =
  let _r = ref 0 in
  Middle.T
