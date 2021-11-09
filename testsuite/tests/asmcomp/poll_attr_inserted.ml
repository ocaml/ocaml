(* TEST
  * setup-ocamlopt.byte-build-env
  ** ocamlopt.byte
ocamlopt_byte_exit_status = "2"
  *** check-ocamlopt.byte-output

  * setup-ocamlopt.opt-build-env
  ** ocamlopt.opt
ocamlopt_opt_exit_status = "2"
  *** check-ocamlopt.opt-output
*)

let[@poll error] c x =
  for c = 0 to 2 do
    ignore(Sys.opaque_identity(42))
  done
