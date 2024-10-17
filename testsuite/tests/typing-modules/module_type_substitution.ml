(* TEST
 expect;
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

module type t3 = t with module type x = sig type t end
[%%expect {|
module type t3 = sig module type x = sig type t end module M : x end
|}]

module type t4 = t with module type x := sig type t end
[%%expect {|
module type t4 = sig module M : sig type t end end
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


module type S = sig
  module M: sig
    module type T
  end
  module N: M.T
end
module type R = S with module type M.T := sig end
[%%expect {|
module type S = sig module M : sig module type T end module N : M.T end
module type R = sig module M : sig end module N : sig end end
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
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       At position "module type t = <here>"
       Module types do not match:
         sig type t = X of x | Y of y end
       is not equal to
         base
       At position "module type t = <here>"
       Type declarations do not match:
         type t = X of x | Y of y
       is not included in
         type t = X of int | Y of float
       1. Constructors do not match:
         "X of x"
       is not the same as:
         "X of int"
       The type "x" is not equal to the type "int"
       2. Constructors do not match:
         "Y of y"
       is not the same as:
         "Y of float"
       The type "y" is not equal to the type "float"
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



module type fst_erased = fst with module type t := sig end
[%%expect {|
Line 1, characters 25-58:
1 | module type fst_erased = fst with module type t := sig end
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "with" constraint "t := sig end" makes a packed module ill-formed.
       (see manual section 12.7.3)
|}]

module type fst_ok = fst with module type t = sig end
[%%expect {|
module type fst_ok = sig module type t = sig end val x : (module t) end
|}]

module type S = sig
  module M: sig
    module type T
  end
  val x: (module M.T)
end

module type R = S with module type M.T := sig end
[%%expect {|
module type S = sig module M : sig module type T end val x : (module M.T) end
Line 8, characters 16-49:
8 | module type R = S with module type M.T := sig end
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "with" constraint "M.T := sig end" makes a packed module ill-formed.
       (see manual section 12.7.3)
|}]


module type S = sig
  module M: sig
    module type T
    val x: (module T)
  end
end

module type R = S with module type M.T := sig end
[%%expect {|
module type S = sig module M : sig module type T val x : (module T) end end
Line 8, characters 16-49:
8 | module type R = S with module type M.T := sig end
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "with" constraint "T := sig end" makes a packed module ill-formed.
       (see manual section 12.7.3)
|}]


(** local module type substitutions *)

module type s = sig
  module type u := sig type a type b type c end
  module type r = sig type r include u end
  module type s = sig include u type a = A end
end
[%%expect {|
module type s =
  sig
    module type r = sig type r type a type b type c end
    module type s = sig type b type c type a = A end
  end
|}]


module type s = sig
  module type u := sig type a type b type c end
  module type wrong = sig type a include u end
end
[%%expect {|
Line 3, characters 33-42:
3 |   module type wrong = sig type a include u end
                                     ^^^^^^^^^
Error: Multiple definition of the type name "a".
       Names must be unique in a given structure or signature.
|}]

(* Only local module type substitutions resulting in paths may
   be used in first class modules. *)

module X = struct
  module type s = sig type t end
  module Y(Z : s) = struct
    module type Ys = sig end
  end
end

module type fcm_path = sig
  module type t_s := X.s
  module Z : sig type t end
  module type t_Ys := X.Y(Z).Ys

  module F : functor (Z : module type of Z) -> sig
    module type t_F = sig type ff end
  end

  module type t_FF := F(Z).t_F

  val x_s: (module t_s)
  val x_sY: (module t_Ys)
  val x_sFF : (module t_FF)
end

[%%expect {|
module X :
  sig
    module type s = sig type t end
    module Y : (Z : s) -> sig module type Ys = sig end end
  end
module type fcm_path =
  sig
    module Z : sig type t end
    module F :
      (Z : sig type t end) -> sig module type t_F = sig type ff end end
    val x_s : (module X.s)
    val x_sY : (module X.Y(Z).Ys)
    val x_sFF : (module F(Z).t_F)
  end
|}]

module type fcm_signature = sig
  module type t := sig end
  val x: (module t)
end
[%%expect {|
Line 3, characters 2-19:
3 |   val x: (module t)
      ^^^^^^^^^^^^^^^^^
Error: The module type "t" is not a valid type for a packed module:
       it is defined as a local substitution (temporary name)
       for an anonymous module type. (see manual section 12.7.3)
|}]


module type hidden = sig
  module type t := sig type u end
  include t
  val x: (module t)
  val x: int
end
[%%expect {|
module type hidden = sig type u val x : int end
|}]


module type s = sig
  module type t := sig end
  type s := (module t)
end
[%%expect {|
Line 3, characters 2-22:
3 |   type s := (module t)
      ^^^^^^^^^^^^^^^^^^^^
Error: The module type "t" is not a valid type for a packed module:
       it is defined as a local substitution (temporary name)
       for an anonymous module type. (see manual section 12.7.3)
|}]

module type s = sig
  module type t := sig end
  module type r := t
  type s := (module r)
end
[%%expect {|
Line 4, characters 2-22:
4 |   type s := (module r)
      ^^^^^^^^^^^^^^^^^^^^
Error: The module type "r" is not a valid type for a packed module:
       it is defined as a local substitution (temporary name)
       for an anonymous module type. (see manual section 12.7.3)
|}]

module type s = sig
  module type t := sig end
  module type r := sig
      type s = (module t)
  end
  module type k = r
end
[%%expect {|
Lines 3-5, characters 2-5:
3 | ..module type r := sig
4 |       type s = (module t)
5 |   end
Error: The module type "t" is not a valid type for a packed module:
       it is defined as a local substitution (temporary name)
       for an anonymous module type. (see manual section 12.7.3)
|}]
