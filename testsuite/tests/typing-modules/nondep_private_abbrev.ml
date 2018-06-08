(* TEST
   * expect
*)

module F(_ : sig end) : sig
  type t = private int
end = struct
  type t = int
end;;
[%%expect{|
module F : sig  end -> sig type t = private int end
|}]

module Direct = F(struct end);;
[%%expect{|
module Direct : sig type t = private int end
|}]

module G(X : sig end) : sig
  type t = F(X).t
end = F(X);;
[%%expect{|
module G : functor (X : sig  end) -> sig type t = F(X).t end
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
module Pub : sig  end -> sig type t = [ `Foo of t ] end
|}]

module Priv(_ : sig end) = struct
  type t = private [ `Foo of t ]
end;;
[%%expect{|
module Priv : sig  end -> sig type t = private [ `Foo of t ] end
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
module H : functor (X : sig  end) -> sig type t = Pub(X).t end
|}]

module I(X : sig end) : sig
  type t = Priv(X).t
end = Priv(X);;
[%%expect{|
module I : functor (X : sig  end) -> sig type t = Priv(X).t end
|}]

module IndirectPub = H(struct end);;
[%%expect{|
module IndirectPub : sig type t = [ `Foo of t ] end
|}]

(* The result would be
   {[
     type t = private [ `Foo of t ]
   ]}
   if we were unrolling the abbrev.  *)
module IndirectPriv = I(struct end);;
[%%expect{|
module IndirectPriv : sig type t = private [ `Foo of 'a ] as 'a end
|}]

