(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-dlambda -stop-after lambda -nopervasives ";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

external p : int -> unit = ""
let () = p 1
