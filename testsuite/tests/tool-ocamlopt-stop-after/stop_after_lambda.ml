(* TEST
* no-flambda
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
    flags = "-dlambda -stop-after lambda -nopervasives "
    ocamlopt_byte_exit_status = "0"
**** check-ocamlopt.byte-output
*)

(* no-flambda: the -lambda output differs with flambda, and
   maintaining two outputs is inconvenient for a test that is not
   really related to code generation. *)

external p : int -> unit = ""
let () = p 1
