(* TEST
flags = " -w a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module type S =
sig
  type a
  type b
end
module Foo
    (Bar : S with type a = private [> `A])
    (Baz : S with type b = private < b : Bar.b ; .. >) =
struct
end
