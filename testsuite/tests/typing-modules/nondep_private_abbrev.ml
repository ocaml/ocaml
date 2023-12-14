(* TEST
 expect;
*)

module F(_ : sig end) : sig
  type t = private int
end = struct
  type t = int
end;;
[%%expect{|
module F : sig end -> sig type t = private int end
|}]

module Direct = F(struct end);;
[%%expect{|
module Direct : sig type t = private int end
|}]

module G(X : sig end) : sig
  type t = F(X).t
end = F(X);;
[%%expect{|
module G : (X : sig end) -> sig type t = F(X).t end
|}]

module Indirect = G(struct end);;
[%%expect{|
module Indirect : sig type t = private int end
|}]

(* unroll_abbrev *)

module Pub(_ : sig end) = struct
  type t = [ `Foo of t ]
end;;
[%%expect{|
module Pub : sig end -> sig type t = [ `Foo of t ] end
|}]

module Priv(_ : sig end) = struct
  type t = private [ `Foo of t ]
end;;
[%%expect{|
module Priv : sig end -> sig type t = private [ `Foo of t ] end
|}]

module DirectPub = Pub(struct end);;
[%%expect{|
module DirectPub : sig type t = [ `Foo of t ] end
|}]

module DirectPriv = Priv(struct end);;
[%%expect{|
module DirectPriv : sig type t = private [ `Foo of t ] end
|}]

module H(X : sig end) : sig
  type t = Pub(X).t
end = Pub(X);;
[%%expect{|
module H : (X : sig end) -> sig type t = Pub(X).t end
|}]

module I(X : sig end) : sig
  type t = Priv(X).t
end = Priv(X);;
[%%expect{|
module I : (X : sig end) -> sig type t = Priv(X).t end
|}]

module IndirectPub = H(struct end);;
[%%expect{|
module IndirectPub : sig type t = [ `Foo of 'a ] as 'a end
|}]

(* The result would be
   {[
     type t = private [ `Foo of t ]
   ]}
   if we were unrolling the abbrev.  *)
module IndirectPriv = I(struct end);;
[%%expect{|
module IndirectPriv : sig type t end
|}]

(* These two behave as though a functor was defined *)
module DirectPrivEta =
  (functor (X : sig end) -> Priv(X))(struct end);;
[%%expect{|
module DirectPrivEta : sig type t end
|}]

module DirectPrivEtaUnit =
  (functor (_ : sig end) -> Priv)(struct end)(struct end);;
[%%expect{|
module DirectPrivEtaUnit : sig type t end
|}]

(*** Test proposed by Jacques in
     https://github.com/ocaml/ocaml/pull/1826#discussion_r194290729 ***)

(* Baseline *)

type t = private [ `Bar of int | `Foo of t -> int ];;
[%%expect{|
type t = private [ `Bar of int | `Foo of t -> int ]
|}]

module M : sig
  type s = private [ `Bar of int | `Foo of 'a -> int ] as 'a
end = struct
  type s = t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type s = t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type s = t end
       is not included in
         sig type s = private [ `Bar of int | `Foo of 'a -> int ] as 'a end
       Type declarations do not match:
         type s = t
       is not included in
         type s = private [ `Bar of int | `Foo of 'a -> int ] as 'a
       The type "[ `Bar of int | `Foo of t -> int ]" is not equal to the type
         "[ `Bar of int | `Foo of 'a -> int ] as 'a"
       Types for tag "`Foo" are incompatible
|}]

(* nondep_type_decl + nondep_type_rec *)

module Priv(_ : sig end) = struct
  type t = private [ `Foo of t -> int | `Bar of int ]
end;;
[%%expect{|
module Priv :
  sig end -> sig type t = private [ `Bar of int | `Foo of t -> int ] end
|}]

module I(X : sig end) : sig
  type t = Priv(X).t
end = Priv(X);;
[%%expect{|
module I : (X : sig end) -> sig type t = Priv(X).t end
|}]

module IndirectPriv = I(struct end);;
[%%expect{|
module IndirectPriv : sig type t end
|}]
