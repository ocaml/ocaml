(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

include struct
  let foo `Test = ()
  let wrap f `Test = f
  let bar = wrap ()
end
