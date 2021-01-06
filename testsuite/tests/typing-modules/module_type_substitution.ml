(* TEST
  * expect
*)

(** Basic *)
module type x = sig type t = int end

module type t = sig
  module type x
  module M:x
end

module type t' = t with module type x = x
[%%expect {|
module type x = sig type t = int end
module type t = sig module type x module M : x end
module type t' = sig module type x = x module M : x end
|}]

module type t'' = t with module type x := x
[%%expect {|
module type t'' = sig module M : x end
|}]


(** nested *)

module type ENDO = sig
  module Inner:
  sig
    module type T
    module F: T -> T
  end
end
module type ENDO_2 = ENDO with module type Inner.T = ENDO
module type ENDO_2' = ENDO with module type Inner.T := ENDO
[%%expect {|
module type ENDO =
  sig module Inner : sig module type T module F : T -> T end end
module type ENDO_2 =
  sig module Inner : sig module type T = ENDO module F : T -> T end end
module type ENDO_2' = sig module Inner : sig module F : ENDO -> ENDO end end
|}]


(** Functor applications *)
module F(X:sig end) = struct module type t end
module Empty = struct end
module type ENDO3 = ENDO with module type Inner.T := F(Empty).t
[%%expect {|
module F : functor (X : sig end) -> sig module type t end
module Empty : sig end
module type ENDO3 =
  sig module Inner : sig module F : F(Empty).t -> F(Empty).t end end
|}]



(** Adding equalities *)

module type base = sig type t = X of int | Y of float end

module type u = sig
  module type t = sig type t = X of int | Y of float end
  module M: t
end

module type s = u with module type t := base
[%%expect {|
module type base = sig type t = X of int | Y of float end
module type u =
  sig module type t = sig type t = X of int | Y of float end module M : t end
module type s = sig module M : base end
|}]




type ext = X of int | Y of float
module type base_ext = base with type t = ext
module type r =
  u with module type t := base_ext
[%%expect {|
type ext = X of int | Y of float
module type base_ext = sig type t = ext = X of int | Y of float end
Line 4, characters 2-34:
4 |   u with module type t := base_ext
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = X of int | Y of float end
       is not included in
         base_ext
       Type declarations do not match:
         type t = X of int | Y of float
       is not included in
         type t = ext = X of int | Y of float
|}]


module type base = sig type t = X of int | Y of float end

module type u = sig
  type x
  type y
  module type t = sig type t = X of x | Y of y end
  module M: t
end

module type r =
  u with type x = int
     and type y = float
     and module type t = base
[%%expect {|
module type base = sig type t = X of int | Y of float end
module type u =
  sig
    type x
    type y
    module type t = sig type t = X of x | Y of y end
    module M : t
  end
module type r =
  sig type x = int type y = float module type t = base module M : t end
|}]

module type r =
  u with type x = int
     and type y = float
     and module type t := base
[%%expect {|
module type r = sig type x = int type y = float module M : base end
|}]


module type r =
  u with type x := int
     and type y := float
     and module type t := base
[%%expect {|
module type r = sig module M : base end
|}]

(** error *)

module type r =
  u with module type t := base

[%%expect {|
Line 4, characters 2-30:
4 |   u with module type t := base
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = X of x | Y of y end
       is not included in
         base
       Type declarations do not match:
         type t = X of x | Y of y
       is not included in
         type t = X of int | Y of float
       Constructors do not match:
         X of x
       is not compatible with:
         X of int
       The types are not equal.
|}]


(** First class module types require an identity *)

module type fst = sig
  module type t
  val x: (module t)
end

module type ext
module type fst_ext = fst with module type t = ext
module type fst_ext = fst with module type t := ext
[%%expect {|
module type fst = sig module type t val x : (module t) end
module type ext
module type fst_ext = sig module type t = ext val x : (module t) end
module type fst_ext = sig val x : (module ext) end
|}]
