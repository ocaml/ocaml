(* TEST
 expect;
*)

module type F = (type a) -> sig
  val default : a
end

module List (type a) = struct
  type t = a list
end

module List2 = (type a) -> struct
  type t = (a * a) list
end

[%%expect{|
module type F = (type a) -> sig val default : a end
module List : (type a) -> sig type t = a list end
module List2 : (type a) -> sig type t = (a * a) list end
|}]

module IntList = List(type int)

module SumList = List(type [`A | `B])

[%%expect{|
module IntList : sig type t = int list end
module SumList : sig type t = [ `A | `B ] list end
|}]

(* Test all cases of wrong applications of modules *)

module Err1 = List(Int)

[%%expect{|
Line 1, characters 14-23:
1 | module Err1 = List(Int)
                  ^^^^^^^^^
Error: The functor was expected to be generative at this position
|}]

module Err2 = List()

[%%expect{|
Line 1, characters 14-20:
1 | module Err2 = List()
                  ^^^^^^
Error: The functor was expected to be applicative at this position
|}]

module type T = sig
  type t
end

module Id (X : T) = X

[%%expect{|
module type T = sig type t end
module Id : functor (X : T) -> sig type t = X.t end
|}]

module Err3 = Id(type int)

(* TODO : correct this error message *)
[%%expect{|
Line 1, characters 14-26:
1 | module Err3 = Id(type int)
                  ^^^^^^^^^^^^
Error: The functor was expected to be generative at this position
|}]

module G () = struct end

module Err4 = G(type int)

[%%expect{|
module G : () -> sig end
Line 3, characters 14-15:
3 | module Err4 = G(type int)
                  ^
Error: This is a generative functor. It can only be applied to "()"
|}]

(* Test coercions *)

module Swaping : (type a) (type b) -> sig
    type t = a
    type s = b
end = (type b) (type a) -> struct
    type t = b
    type s = a
end

[%%expect{|
module Swaping : functor (type a) (type b) -> sig type t = a type s = b end
|}]

module Err5 : (type t) -> sig type nonrec t = t end = Id

[%%expect{|
|}]
