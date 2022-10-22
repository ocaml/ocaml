(* TEST
   * expect
*)

(* PR#6394 *)

module rec X : sig
 type t = int * bool
end = struct
 type t = A | B
 let f = function A | B -> 0
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |  type t = A | B
5 |  let f = function A | B -> 0
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t = A | B val f : t -> int end
       is not included in
         sig type t = int * bool end
       Type declarations do not match:
         type t = X.t = A | B
       is not included in
         type t = int * bool
       The type X.t is not equal to the type int * bool
|}];;


(* PR#7838 *)

module Make (X : sig val f : [ `A ] -> unit end) = struct
 let make f1 f2 arg = match arg with `A -> f1 arg; f2 arg
 let f = make X.f (fun _ -> ())
end;;
[%%expect{|
module Make :
  functor (X : sig val f : [ `A ] -> unit end) ->
    sig
      val make : (([< `A ] as 'a) -> 'b) -> ('a -> 'c) -> 'a -> 'c
      val f : [ `A ] -> unit
    end
|}]


(* reexport *)
type ('a,'b) def = X of int constraint 'b = [> `A]

type arity = (int, [`A]) def = X of int;;
[%%expect{|
type ('a, 'b) def = X of int constraint 'b = [> `A ]
Line 3, characters 0-39:
3 | type arity = (int, [`A]) def = X of int;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         (int, [ `A ]) def
       They have different arities.
|}]

type ('a,'b) ct = (int,'b) def = X of int;;
[%%expect{|
Line 1, characters 0-41:
1 | type ('a,'b) ct = (int,'b) def = X of int;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         (int, [> `A ]) def
       Their parameters differ
       The type int is not equal to the type 'a
|}]

type ('a,'b) kind = ('a, 'b) def = {a:int} constraint 'b = [> `A];;
[%%expect{|
Line 1, characters 0-65:
1 | type ('a,'b) kind = ('a, 'b) def = {a:int} constraint 'b = [> `A];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         ('a, [> `A ]) def
       The original is a variant, but this is a record.
|}]

type d = X of int | Y of int

type missing = d = X of int
[%%expect{|
type d = X of int | Y of int
Line 3, characters 0-27:
3 | type missing = d = X of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       An extra constructor, Y, is provided in the original definition.
|}]

type wrong_type = d = X of float
[%%expect{|
Line 1, characters 0-32:
1 | type wrong_type = d = X of float
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       1. Constructors do not match:
         X of int
       is not the same as:
         X of float
       The type int is not equal to the type float
       2. An extra constructor, Y, is provided in the original definition.
|}]

type mono = Foo of float
type unboxed = mono = Foo of float [@@unboxed]
[%%expect{|
type mono = Foo of float
Line 2, characters 0-46:
2 | type unboxed = mono = Foo of float [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type mono
       Their internal representations differ:
       this definition uses unboxed representation.
|}]

type perm = d = Y of int | X of int
[%%expect{|
Line 1, characters 0-35:
1 | type perm = d = Y of int | X of int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type d
       Constructors X and Y have been swapped.
|}]

module M : sig
  type t = Foo of int
end = struct
  type t = Foo : int -> t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Foo : int -> t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo : int -> t end
       is not included in
         sig type t = Foo of int end
       Type declarations do not match:
         type t = Foo : int -> t
       is not included in
         type t = Foo of int
       Constructors do not match:
         Foo : int -> t
       is not the same as:
         Foo of int
       The first has explicit return type and the second doesn't.
|}]
