(* TEST
readonly_files = "pr3918a.mli pr3918b.mli"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "pr3918a.mli"
*** ocamlc.byte
module = "pr3918b.mli"
**** script
script = "rm -f pr3918a.cmi"
***** ocamlc.byte
module = "pr3918c.ml"
ocamlc_byte_exit_status = "2"
***** check-ocamlc.byte-output
*)

(*
  ocamlc -c pr3918a.mli pr3918b.mli
  rm -f pr3918a.cmi
  ocamlc -c pr3918c.ml
*)

open Pr3918b

let f x = (x : 'a vlist :> 'b vlist)
let f (x : 'a vlist) = (x : 'b vlist)
