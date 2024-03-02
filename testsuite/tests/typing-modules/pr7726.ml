(* TEST
 expect;
*)

module type T = sig type t end
module Fix(F:(T -> T)) = struct
  module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
end;;
[%%expect{|
module type T = sig type t end
module Fix :
  (F : T -> T) -> sig module rec Fixed : sig type t = F(Fixed).t end end
|}]

module T1 = Fix(functor (X:sig type t end) -> struct type t = X.t option end);;
[%%expect{|
Line 1, characters 12-77:
1 | module T1 = Fix(functor (X:sig type t end) -> struct type t = X.t option end);;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       The definition of "Fixed.t" contains a cycle:
         "F(Fixed).t" = "Fixed.t option",
         "Fixed.t option" contains "Fixed.t",
         "Fixed.t" = "F(Fixed).t",
         "F(Fixed).t" = "Fixed.t option"
|}]
module T2 = Fix(functor (X:sig type t end) -> struct type t = X.t end);;
[%%expect{|
Line 1, characters 12-70:
1 | module T2 = Fix(functor (X:sig type t end) -> struct type t = X.t end);;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       The definition of "Fixed.t" contains a cycle:
         "F(Fixed).t" = "Fixed.t",
         "Fixed.t" = "F(Fixed).t",
         "F(Fixed).t" = "Fixed.t"
|}]

(* Positive example *)
module F3(X:T) = struct type t = Z | S of X.t end;;
module T3 = Fix(F3);;
let x : T3.Fixed.t = S Z;;
[%%expect{|
module F3 : (X : T) -> sig type t = Z | S of X.t end
module T3 : sig module rec Fixed : sig type t = F3(Fixed).t end end
val x : T3.Fixed.t = F3(T3.Fixed).S F3(T3.Fixed).Z
|}]

(* Torture the type checker more *)
module M = struct
  module F (X : T) : T = X
  module rec Fixed : sig type t = F(Fixed).t end = Fixed
end
module type S = module type of M
module Id (X : T) = X;;
[%%expect{|
module M :
  sig
    module F : (X : T) -> T
    module rec Fixed : sig type t = F(Fixed).t end
  end
module type S =
  sig
    module F : (X : T) -> T
    module rec Fixed : sig type t = F(Fixed).t end
  end
module Id : (X : T) -> sig type t = X.t end
|}]

module type Bad = S with module F = Id;;
[%%expect{|
Line 1, characters 18-38:
1 | module type Bad = S with module F = Id;;
                      ^^^^^^^^^^^^^^^^^^^^
Error: In this instantiated signature:
       The definition of "Fixed.t" contains a cycle:
         "F(Fixed).t" = "Fixed.t",
         "Fixed.t" = "F(Fixed).t",
         "F(Fixed).t" = "Fixed.t"
|}]

(* More examples by lpw25 *)
module M = Fix(Id);;
[%%expect{|
Line 1, characters 11-18:
1 | module M = Fix(Id);;
               ^^^^^^^
Error: In the signature of this functor application:
       The definition of "Fixed.t" contains a cycle:
         "Id(Fixed).t" = "Fixed.t",
         "Fixed.t" = "Id(Fixed).t",
         "Id(Fixed).t" = "Fixed.t"
|}]
type t = Fix(Id).Fixed.t;;
[%%expect{|
Line 1, characters 9-24:
1 | type t = Fix(Id).Fixed.t;;
             ^^^^^^^^^^^^^^^
Error: In the signature of Fix(Id):
       The definition of "Fixed.t" contains a cycle:
         "Id(Fixed).t" = "Fixed.t",
         "Fixed.t" = "Id(Fixed).t",
         "Id(Fixed).t" = "Fixed.t"
|}]
let f (x : Fix(Id).Fixed.t) = x;;
[%%expect{|
Line 1, characters 11-26:
1 | let f (x : Fix(Id).Fixed.t) = x;;
               ^^^^^^^^^^^^^^^
Error: In the signature of Fix(Id):
       The definition of "Fixed.t" contains a cycle:
         "Id(Fixed).t" = "Fixed.t",
         "Fixed.t" = "Id(Fixed).t",
         "Id(Fixed).t" = "Fixed.t"
|}]

module Foo (F : T -> T) = struct
    let f (x : Fix(F).Fixed.t) = x
  end
module M = Foo(Id);;
M.f 5;;
[%%expect{|
module Foo : (F : T -> T) -> sig val f : Fix(F).Fixed.t -> Fix(F).Fixed.t end
module M : sig val f : Fix(Id).Fixed.t -> Fix(Id).Fixed.t end
Line 1:
Error: In the signature of Fix(Id):
       The definition of "Fixed.t" contains a cycle:
         "Id(Fixed).t" = "Fixed.t",
         "Fixed.t" = "Id(Fixed).t",
         "Id(Fixed).t" = "Fixed.t"
|}]

(* Extra tests for GPR#1676 *)
module F() = struct type t end
module M = struct end;;
type t = F(M).t;;
[%%expect{|
module F : () -> sig type t end
module M : sig end
Line 3, characters 9-15:
3 | type t = F(M).t;;
             ^^^^^^
Error: The functor "F" is generative, it cannot be applied in type expressions
|}]

module Fix2(F:(T -> T)) = struct
  module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
  module R(X:sig end) = struct type t = Fixed.t end
end;;
let f (x : Fix2(Id).R(M).t) = x;;
[%%expect{|
module Fix2 :
  (F : T -> T) ->
    sig
      module rec Fixed : sig type t = F(Fixed).t end
      module R : (X : sig end) -> sig type t = Fixed.t end
    end
Line 5, characters 11-26:
5 | let f (x : Fix2(Id).R(M).t) = x;;
               ^^^^^^^^^^^^^^^
Error: In the signature of Fix2(Id):
       The definition of "Fixed.t" contains a cycle:
         "Id(Fixed).t" = "Fixed.t",
         "Fixed.t" = "Id(Fixed).t",
         "Id(Fixed).t" = "Fixed.t"
|}]
