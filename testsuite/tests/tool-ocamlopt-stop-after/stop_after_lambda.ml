(* TEST
* setup-ocamlopt.byte-build-env
** ocamlopt.byte
    flags = "-dlambda -stop-after lambda -nopervasives "
    ocamlopt_byte_exit_status = "0"
*** check-ocamlopt.byte-output
*)

external p : int -> unit = ""
let () = p 1
