(* TEST
flags = " -w -a -no-alias-deps"
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module A = MissingModule
let () = let open A in x
