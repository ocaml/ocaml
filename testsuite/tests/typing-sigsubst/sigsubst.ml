(* TEST
   * expect
*)

module type Printable = sig
  type t
  val print : t -> unit
end
[%%expect {|
module type Printable = sig type t val print : t -> unit end
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
Line 3, characters 2-36:
3 |   include Comparable with type t = t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Illegal shadowing of included type t/285 by t/290
       Line 2, characters 2-19:
         Type t/285 came from this include
       Line 3, characters 2-23:
         The value print has no valid type if t/285 is shadowed
|}]

module type Sunderscore = sig
  type (_, _) t
end with type (_, 'a) t = int * 'a
[%%expect {|
module type Sunderscore = sig type (_, 'a) t = int * 'a end
|}]


(* Valid substitutions in a recursive module used to fail
   due to the ordering of the modules. This is fixed since #9623. *)
module type S0 = sig
  module rec M : sig type t = M2.t end
  and M2 : sig type t = int end
end with type M.t = int
[%%expect {|
module type S0 =
  sig module rec M : sig type t = int end and M2 : sig type t = int end end
|}]


module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig type t val print : t -> unit val compare : t -> t -> int end
|}]
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig type t val print : t -> unit val compare : t -> t -> int end
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
module type S3 = S with type _ t := int
[%%expect {|
module type S3 = sig val map : ('a -> 'b) -> int -> int end
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
module type S = sig end
|}]

module type S = sig
  type t = [ `Foo ]
  type s = private [< t ]
end with type t := [ `Foo ]
[%%expect {|
module type S = sig type s = private [< `Foo ] end
|}]

module type S = sig
  type t = ..
  type t += A
end with type t := exn
[%%expect {|
module type S = sig type exn += A end
|}]

(* We allow type constraints when replacing a path by a path. *)
type 'a t constraint 'a = 'b list
module type S = sig
  type 'a t2 constraint 'a = 'b list
  type 'a mylist = 'a list
  val x : int mylist t2
end with type 'a t2 := 'a t
[%%expect {|
type 'a t constraint 'a = 'b list
module type S = sig type 'a mylist = 'a list val x : int mylist t end
|}]

(* but not when replacing a path by a type expression *)
type 'a t constraint 'a = 'b list
module type S = sig
  type 'a t2 constraint 'a = 'b list
  type 'a mylist = 'a list
  val x : int mylist t2
end with type 'a t2 := 'a t * bool
[%%expect {|
type 'a t constraint 'a = 'b list
Lines 2-6, characters 16-34:
2 | ................sig
3 |   type 'a t2 constraint 'a = 'b list
4 |   type 'a mylist = 'a list
5 |   val x : int mylist t2
6 | end with type 'a t2 := 'a t * bool
Error: Destructive substitutions are not supported for constrained
       types (other than when replacing a type constructor with
       a type constructor with the same arguments).
|}]

(* Issue where the typer weakens an alias, which breaks the typing of the rest
   of the signature. (MPR#7723)*)
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
    module M2 = M1
    module M3 : sig module M = M2 end
    module F : functor (X : sig module M = M1 end) -> sig type t end
    type t = F(M3).t
  end
|}]

type (_, _) eq = Refl : ('a, 'a) eq

module Equal (M : Set.OrderedType) (N : Set.OrderedType with type t = M.t) : sig
  val eq : (Set.Make(M).t, Set.Make(N).t) eq
end = struct
  type meq = Eq of (Set.Make(M).t, Set.Make(M).t) eq
  module type S = sig
    module N = M
    type neq = meq = Eq of (Set.Make(M).t, Set.Make(N).t) eq
  end
  module type T = S with type N.t = M.t with module N := N;;
  module rec T : T = T
  let eq =
    let T.Eq eq = Eq Refl in
    eq
end;;
[%%expect {|
type (_, _) eq = Refl : ('a, 'a) eq
Line 11, characters 18-58:
11 |   module type T = S with type N.t = M.t with module N := N;;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of N
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = M.t val compare : t -> t -> int end
       is not included in
         (module M)
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

(* Regression test: at some point, expanding S1 twice in the same
   "with type" would result in a signature with duplicate ids, which
   would confuse the rewriting (we would end with (M2.x : int)) and
   only then get refreshened. *)
module type S = sig
  module type S1 = sig type t type a val x : t end
  module M1 : S1
  type a = M1.t
  module M2 : S1
  type b = M2.t
end with type M1.a = int and type M2.a = int and type M1.t := int;;
[%%expect {|
module type S =
  sig
    module type S1 = sig type t type a val x : t end
    module M1 : sig type a = int val x : int end
    type a = int
    module M2 : sig type t type a = int val x : t end
    type b = M2.t
  end
|}]

(* And now some corner cases with aliases: *)

module type S = sig
  module M : sig type t end
  module A = M
end with type M.t := float
[%%expect {|
Lines 1-4, characters 16-26:
1 | ................sig
2 |   module M : sig type t end
3 |   module A = M
4 | end with type M.t := float
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
Line 1, characters 17-41:
1 | module type S2 = S with type M.t := float
                     ^^^^^^^^^^^^^^^^^^^^^^^^
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

(* In the presence of recursive modules, the use of a module can come before its
   definition (in the typed tree). *)

module Id(X : sig type t end) = struct type t = X.t end
module type S3 = sig
  module rec M : sig type t = A of Id(M2).t end
  and M2 : sig type t end
end with type M2.t := int
[%%expect {|
module Id : functor (X : sig type t end) -> sig type t = X.t end
Lines 2-5, characters 17-25:
2 | .................sig
3 |   module rec M : sig type t = A of Id(M2).t end
4 |   and M2 : sig type t end
5 | end with type M2.t := int
Error: This `with' constraint on M2.t makes the applicative functor
       type Id(M2).t ill-typed in the constrained signature:
       Modules do not match: sig end is not included in sig type t end
       The type `t' is required but not provided
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
module type S = sig module M : sig end type t = A.P.t end
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
Lines 1-10, characters 16-24:
 1 | ................sig
 2 |   module M : sig
 3 |     module N : sig
 4 |       module P : sig
 5 |         type t
 6 |       end
 7 |     end
 8 |   end
 9 |   module Alias = M
10 | end with module M.N := A
Error: This `with' constraint on M.N changes M, which is aliased
       in the constrained signature (as Alias).
|}]
