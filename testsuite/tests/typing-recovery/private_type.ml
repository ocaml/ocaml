(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module T : sig
  type t = private int
  type r = private Foo of int
  val mk : int -> t
end = struct
  type t = int
  type r = Foo of int
  let mk x = x
end

type k = private A

let r = A

type t = {a : T.t; b : int}

let a = T.mk 12
let b = (10 : T.t)


type t' = private <foo : int>

let f =
  let a = T.Foo 10 in
  a, 10

let x = {a = 10; b = (a :> int) + 15}
