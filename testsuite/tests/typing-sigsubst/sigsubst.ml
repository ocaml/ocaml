module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end
[%%expect {|
module type Printable =
  sig type t val print : Format.formatter -> t -> unit end
|}]
module type Comparable = sig
  type t
  val compare : t -> t -> int
end
[%%expect {|
module type Comparable = sig type t val compare : t -> t -> int end
|}]
module type PrintableComparable = sig
  include Printable
  include Comparable with type t = t
end
[%%expect {|
Line _, characters 2-36:
Error: Multiple definition of the type name t.
       Names must be unique in a given structure or signature.
|}]

module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig
    type t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
|}]
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig
    type t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
|}]
module type ComparableInt = Comparable with type t := int
[%%expect {|
module type ComparableInt = sig val compare : int -> int -> int end
|}]
module type S = sig type t val f : t -> t end
[%%expect {|
module type S = sig type t val f : t -> t end
|}]
module type S' = S with type t := int
[%%expect {|
module type S' = sig val f : int -> int end
|}]

module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type S1 = S with type 'a t := 'a list
[%%expect {|
module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type S1 = sig val map : ('a -> 'b) -> 'a list -> 'b list end
|}]
module type S2 = S with type 'a t := (string * 'a) list
[%%expect {|
module type S2 =
  sig val map : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list end
|}]


module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end
module M = struct type exp = string type arg = int end
module type S' = S with module T := M
[%%expect {|
module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end
module M : sig type exp = string type arg = int end
module type S' = sig val f : M.exp -> M.arg end
|}]


module type S = sig type 'a t end with type 'a t := unit
[%%expect {|
module type S = sig  end
|}]

(* Issue where the typer expands an alias, which breaks the typing of the rest
   of the signature, but no error is given to the user. *)
module type S = sig
  module M1 : sig type t = int end
  module M2 = M1
  module M3 : sig module M = M2 end
  module F(X : sig module M = M1 end) : sig type t end
  type t = F(M3).t
end with type M2.t = int
[%%expect {|
module type S =
  sig
    module M1 : sig type t = int end
    module M2 : sig type t = int end
    module M3 : sig module M = M2 end
    module F : functor (X : sig module M = M1 end) -> sig type t end
    type t = F(M3).t
  end
|}]

(* Checking that the uses of M.t are rewritten regardless of how they
   are named, but we don't rewrite other types by the same name. *)
module type S = sig
  module M : sig type t val x : t end
  val y : M.t
  module A : sig module M : sig type t val z : t -> M.t end end
end with type M.t := float
[%%expect {|
module type S =
  sig
    module M : sig val x : float end
    val y : float
    module A : sig module M : sig type t val z : t -> float end end
  end
|}]

(* And now some corner cases with aliases: *)

module type S = sig
  module M : sig type t end
  module A = M
end with type M.t := float
[%%expect {|
Line _, characters 16-89:
Error: This `with' constraint on M.t changes M, which is aliased
       in the constrained signature (as A).
|}]

(* And more corner cases with applicative functors: *)

module type S = sig
  module M : sig type t type u end
  module F(X : sig type t end) : sig type t end
  type t = F(M).t
end
[%%expect {|
module type S =
  sig
    module M : sig type t type u end
    module F : functor (X : sig type t end) -> sig type t end
    type t = F(M).t
  end
|}]

(* This particular substitution cannot be made to work *)
module type S2 = S with type M.t := float
[%%expect {|
Line _, characters 17-41:
Error: This `with' constraint on M.t makes the applicative functor
       type F(M).t ill-typed in the constrained signature:
       Modules do not match:
         sig type u = M.u end
       is not included in
         sig type t end
       The type `t' is required but not provided
|}]

(* However if the applicative functor doesn't care about the type
   we're removing, the typer accepts the removal. *)
module type S2 = S with type M.u := float
[%%expect {|
module type S2 =
  sig
    module M : sig type t end
    module F : functor (X : sig type t end) -> sig type t end
    type t = F(M).t
  end
|}]

(* Deep destructive module substitution: *)

module A = struct module P = struct type t let x = 1 end end
module type S = sig
  module M : sig
    module N : sig
      module P : sig
        type t
      end
    end
  end
  type t = M.N.P.t
end with module M.N := A
[%%expect {|
module A : sig module P : sig type t val x : int end end
module type S = sig module M : sig  end type t = A.P.t end
|}]

(* Same as for types, not all substitutions are accepted *)

module type S = sig
  module M : sig
    module N : sig
      module P : sig
        type t
      end
    end
  end
  module Alias = M
end with module M.N := A
[%%expect {|
Line _, characters 16-159:
Error: This `with' constraint on M.N changes M, which is aliased
       in the constrained signature (as Alias).
|}]
