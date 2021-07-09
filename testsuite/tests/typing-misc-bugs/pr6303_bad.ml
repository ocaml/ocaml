(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

type 'a  foo = {x: 'a; y: int}
let r = {{x = 0; y = 0} with x = 0}
let r' : string foo = r
