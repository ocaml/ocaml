(* TEST_BELOW





*)

(* Bad (t = int * t) *)
module rec A : sig type t = int * A.t end = struct type t = int * A.t end;;

(* TEST
{
  flags = " -w -a ";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
