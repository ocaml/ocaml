(* TEST
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

type accepted = Foo of (int -> int)

type wrongly_rejected = Foo of int -> int
