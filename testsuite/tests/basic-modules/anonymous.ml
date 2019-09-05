(* TEST
flags = "-c -nostdlib -nopervasives -dlambda -dno-unique-ids"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** check-ocamlopt.byte-output
*)

module _ = struct
  let x = 13, 37
end

module rec A : sig
  type t = B.t
end = A
and _ : sig
  type t = A.t
  val x : int * int
end = struct
  type t = B.t
  let x = 4, 2
end
and B : sig
  type t
end = struct
  type t

  let x = "foo", "bar"
end
