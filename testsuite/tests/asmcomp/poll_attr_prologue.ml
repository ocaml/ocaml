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

let[@poll error] rec c x l =
  match l with
  | [] -> 0
  | _ :: tl -> (c[@tailcall]) (x+1) tl
