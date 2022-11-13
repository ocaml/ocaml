(* TEST
   ocamlc_byte_exit_status = "0"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

let () = let z = 1 in ()

# 1000
let () = let z = 1 in ()

# 20 "test-line-number-directiveXXX.ml"
let () = let z = 1 in ()

# 1000
let () = let z = 1 in ()
