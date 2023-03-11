(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
    flags = "-dlambda -stop-after lambda -nopervasives "
    ocamlc_byte_exit_status = "0"
*** check-ocamlc.byte-output
*)

external p : int -> unit = ""
let () = p 1
