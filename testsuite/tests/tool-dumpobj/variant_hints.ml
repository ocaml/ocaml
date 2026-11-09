(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints on the tests of whether a value of a variant type is an
   immediate *)

external is_int : 'a -> bool = "%obj_is_int"

type t = A | B | C of int

(* Constructors: the immediates are the constant constructors *)
let constructor x = match x with A -> 0 | B -> 1 | C n -> n

(* Polymorphic variants *)
let polymorphic_variant x = match x with `A -> 0 | `B n -> n

(* Any value: no hint *)
let any x = is_int x
