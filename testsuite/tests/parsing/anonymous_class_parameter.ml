(* TEST
   flags = "-i"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

(* This test is valid OCaml code.
   It uses an anonymous type variable as a formal parameter in a class
   declaration. This used to be rejected by the parser, even though the
   printer (ocamlc -i) could in fact produce it. *)

class ['a, _] foo = object
  method bar: 'a -> 'a = fun x -> x
end
