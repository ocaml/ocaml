(* TEST
 flags = " -short-paths ";
 ocamlrunparam += "l=100000";
 expect;
*)

(* These tests may run forever on failure. ocamlrunparam+="l=100000" limits stack size to cut them short. *)

(* Immediate cycle *)

module Test1 = struct
  module rec X : sig type t = X.t end = struct type t = int end
end
[%%expect {|
Line 2, characters 2-63:
2 |   module rec X : sig type t = X.t end = struct type t = int end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "X.t" is cyclic:
         "X.t" = "X.t"
|}]


(* 2-cycle *)

module Test2 = struct
  module rec X : sig type t = Y.t end = struct type t = Y.t end
  and Y : sig type t = X.t end = struct type t = int end
end
[%%expect {|
Line 2, characters 2-63:
2 |   module rec X : sig type t = Y.t end = struct type t = Y.t end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "X.t" contains a cycle:
         "Y.t" = "X.t",
         "X.t" = "Y.t",
         "Y.t" = "X.t"
|}]


(* Cycle with exactly equal parameters *)

module Test3 = struct
  module rec X : sig
    type ('a, 'b) t = ('a, 'b) Y.t
  end = struct
    type ('a, 'b) t = ('a, 'b) Y.t
  end
  and Y : sig
    type ('a, 'b) t = ('a, 'b) X.t
  end = struct
    type ('a, 'b) t = int
  end
end
[%%expect{|
Lines 2-6, characters 2-5:
2 | ..module rec X : sig
3 |     type ('a, 'b) t = ('a, 'b) Y.t
4 |   end = struct
5 |     type ('a, 'b) t = ('a, 'b) Y.t
6 |   end
Error: The definition of "X.t" contains a cycle:
         "('a, 'b) Y.t" = "('a, 'b) X.t",
         "('a, 'b) X.t" = "('a, 'b) Y.t",
         "('a, 'b) Y.t" = "('a, 'b) X.t"
|}]

(* Cycle with unequal parameters *)

module Test4 = struct
  module rec X : sig
    type ('a, 'b) t = ('b, 'a) Y.t
  end = struct
    type ('a, 'b) t = ('b, 'a) Y.t
  end
  and Y : sig
    type ('a, 'b) t = ('b, 'a) X.t
  end = struct
    type ('a, 'b) t = int
  end
end
[%%expect{|
Lines 2-6, characters 2-5:
2 | ..module rec X : sig
3 |     type ('a, 'b) t = ('b, 'a) Y.t
4 |   end = struct
5 |     type ('a, 'b) t = ('b, 'a) Y.t
6 |   end
Error: The definition of "X.t" contains a cycle:
         "('a, 'b) Y.t" = "('b, 'a) X.t",
         "('b, 'a) X.t" = "('a, 'b) Y.t",
         "('a, 'b) Y.t" = "('b, 'a) X.t"
|}]

(* Cycle with unequal number of parameters *)

module Test5 = struct
  module rec X : sig
    type ('a, 'b) t = ('b, 'a, bool) Y.t
  end = struct
    type ('a, 'b) t = ('b, 'a, bool) Y.t
  end
  and Y : sig
    type ('a, 'b, 'c) t = ('b, 'a) X.t
  end = struct
    type ('a, 'b, 'c) t = int
  end
end
[%%expect{|
Lines 2-6, characters 2-5:
2 | ..module rec X : sig
3 |     type ('a, 'b) t = ('b, 'a, bool) Y.t
4 |   end = struct
5 |     type ('a, 'b) t = ('b, 'a, bool) Y.t
6 |   end
Error: The definition of "X.t" contains a cycle:
         "('a, 'b, bool) Y.t" = "('b, 'a) X.t",
         "('b, 'a) X.t" = "('a, 'b, bool) Y.t",
         "('a, 'b, bool) Y.t" = "('b, 'a) X.t"
|}]

(* Cycle is more than just aliasing *)

module rec A : sig type t = B.t -> int end = struct type t = B.t -> int end
       and B : sig type t = A.t end = struct type t = A.t end
[%%expect {|
Line 1, characters 0-75:
1 | module rec A : sig type t = B.t -> int end = struct type t = B.t -> int end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "A.t" contains a cycle:
         "B.t -> int" contains "B.t",
         "B.t" = "A.t",
         "A.t" = "B.t -> int",
         "B.t -> int" contains "B.t",
         "B.t" = "A.t"
|}]

(* Short paths are actually short *)

type a__name__that__shall__not__be__printed
module type T = sig type t end
module Fix(F: T -> T) = struct
  module Atlas = struct type t = a__name__that__shall__not__be__printed end
  module rec Fixed: sig
    type t = F(Fixed).t
  end = F(Fixed)
end
module Err = Fix(functor (X:T) -> struct type t = a__name__that__shall__not__be__printed -> X.t end)
[%%expect{|
type a__name__that__shall__not__be__printed
module type T = sig type t end
module Fix :
  (F : T -> T) ->
    sig
      module Atlas : sig type t = a__name__that__shall__not__be__printed end
      module rec Fixed : sig type t = F(Fixed).t end
    end
Line 9, characters 13-100:
9 | module Err = Fix(functor (X:T) -> struct type t = a__name__that__shall__not__be__printed -> X.t end)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       The definition of "Fixed.t" contains a cycle:
         "Fixed.t" = "Atlas.t -> Fixed.t",
         "Atlas.t -> Fixed.t" contains "Fixed.t",
         "Fixed.t" = "Fixed.t",
         "Fixed.t" = "Atlas.t -> Fixed.t"
|}]

module Constraint(F:sig type 'a t end-> sig type 'a t end) = struct
  type 'a x = 'b constraint 'a = 'b * 'b
  module rec Fixed: sig
    type 'a s = < x: 'a F(Fixed).t >
    type 'a t = ('a  * 'a) x s
  end = Fixed
end
[%%expect {|
module Constraint :
  (F : sig type 'a t end -> sig type 'a t end) ->
    sig
      type 'a x = 'b constraint 'a = 'b * 'b
      module rec Fixed :
        sig type 'a s = < x : 'a F(Fixed).t > type 'a t = ('a * 'a) x s end
    end
|}]

module Ok = Constraint(functor (X:sig type 'a t end) -> X)
[%%expect {|
module Ok :
  sig
    type 'a x = 'b constraint 'a = 'b * 'b
    module rec Fixed :
      sig type 'a s = < x : 'a Fixed.t > type 'a t = ('a * 'a) x s end
  end
|}]
