(* TEST
   flags = "-dparsetree"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

let%foo x = 42
let%foo _ = () and _ = ()
let%foo _ = ()
