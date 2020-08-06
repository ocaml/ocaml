(* TEST
   ocamlc_byte_exit_status = "2"
   ocamlopt_byte_exit_status = "2"
   flags="-json"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
   * setup-ocamlopt.byte-build-env
   ** ocamlopt.byte
   *** check-ocamlopt.byte-output
 *)

let a = ;;
