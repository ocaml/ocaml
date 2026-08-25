(* TEST
 expect;
*)

module M = struct
  type t
end;;
[%%expect{|
module M : sig type t end
|}]

module type Accepted = sig
  type t := int

  type 'a u := 'a list

  type 'a v := (string * 'a) list

  module T := M

  type _ w := T.t

  val f : t u -> char w v
end;;
[%%expect{|
module type Accepted = sig val f : int list -> (string * M.t) list end
|}]

module F(X : sig type t end) = struct
  type t = X.t
end;;
[%%expect{|
module F : (X : sig type t end) -> sig type t = X.t end
|}]

module type Accepted2 = sig
  module N := F(M)

  val foo : N.t -> int
end;;
[%%expect{|
module type Accepted2 = sig val foo : F(M).t -> int end
|}]

module type Reject1 = sig
  module M := Funct(M)
end;;
[%%expect{|
Line 2, characters 14-22:
2 |   module M := Funct(M)
                  ^^^^^^^^
Error: Unbound module "Funct"
Hint:    Did you mean "Fun"?
|}]

module type Reject2 = sig
  module M := F(N)
end;;
[%%expect{|
Line 2, characters 16-17:
2 |   module M := F(N)
                    ^
Error: Unbound module "N"
|}]

module type Reject3 = sig
  type t := u
end;;
[%%expect{|
Line 2, characters 12-13:
2 |   type t := u
                ^
Error: Unbound type constructor "u"
|}]

module type RejectRec = sig
  type t := [ `Foo of t | `Nil ]
end;;
[%%expect{|
Line 2, characters 22-23:
2 |   type t := [ `Foo of t | `Nil ]
                          ^
Error: Unbound type constructor "t"
|}]

module type AcceptAnd = sig
  type t := int
  and u := int * int
end;;
[%%expect{|
module type AcceptAnd = sig end
|}]

module type RejectAnd = sig
  type t := int
  and u := t * int
end;;
[%%expect{|
Line 3, characters 11-12:
3 |   and u := t * int
               ^
Error: Unbound type constructor "t"
|}]

type ('a, 'b) foo = Foo

type 'a s = 'b list constraint 'a = (int, 'b) foo

module type S = sig
  type 'a t := 'a s * bool
  type 'a bar = (int, 'a) foo
  val x : string bar t
end
[%%expect{|
type ('a, 'b) foo = Foo
type 'a s = 'b list constraint 'a = (int, 'b) foo
Line 6, characters 2-26:
6 |   type 'a t := 'a s * bool
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Destructive substitutions are not supported for constrained
       types (other than when replacing a type constructor with
       a type constructor with the same arguments).
|}]

(* #12745: on a non-aliasable module substitution, warning 60 used to
   report a location in the source file where the substituted module
   was originally declared instead of the substitution site, and the
   substituted module's [md_uid] was shared with the original module's
   uid so that the unused-module check never fired for the
   single-compilation-unit repro. The test below pins the post-fix
   behavior: the warning fires at the substitution site in both default
   and principal modes. *)
[@@@warning "+60"]

module A = struct
  module type S = sig
    module Foo : sig end
  end
end;;
[%%expect{|
module A : sig module type S = sig module Foo : sig end end end
|}]

module type T = sig
  module G (X : A.S) : sig
    module Bar := X.Foo
  end
end;;
[%%expect{|
Line 3, characters 4-23:
3 |     module Bar := X.Foo
        ^^^^^^^^^^^^^^^^^^^
Warning 60 [unused-module]: unused module "Bar".

module type T = sig module G : (X : A.S) -> sig end end
|}]
