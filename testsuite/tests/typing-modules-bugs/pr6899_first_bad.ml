(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

let should_reject =
  let table = Hashtbl.create 1 in
  fun x y -> Hashtbl.add table x y
