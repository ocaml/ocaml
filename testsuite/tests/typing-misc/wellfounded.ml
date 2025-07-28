(* TEST
 expect;
*)

(* PR#6768 *)

type _ prod = Prod : ('a * 'y) prod;;

let f : type t. t prod -> _ = function Prod ->
  let module M =
    struct
      type d = d * d
    end
  in ()
;;
[%%expect{|
type _ prod = Prod : ('a * 'y) prod
Line 6, characters 6-20:
6 |       type d = d * d
          ^^^^^^^^^^^^^^
Error: The definition of "d" contains a cycle:
         "d" = "d * d",
         "d * d" contains "d"
|}];;

(* #9314 by ccasin/alpha-convert: *)
type t = int s
and 'a s = 'a constraint 'a = t
;;
[%%expect{|
Line 2, characters 0-31:
2 | and 'a s = 'a constraint 'a = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "s" contains a cycle:
         the 1st type parameter of "s" is constrained to "t",
         "t" = "int s"
|}];;

(* #9314 by ccasin/alpha-convert: *)
type t = int s
and 'a s = 'a id constraint 'a = t
and 'a id = 'a
;;
[%%expect{|
Line 2, characters 0-34:
2 | and 'a s = 'a id constraint 'a = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "s" contains a cycle:
         the 1st type parameter of "s" is constrained to "t",
         "t" = "int s"
|}];;

type 'a z0 = 'a constraint 'a = t
and t = int s z1
and 'a s = 'a constraint 'a = t
and 'a z1 = 'a constraint 'a = t
;;
[%%expect{|
Line 4, characters 0-32:
4 | and 'a z1 = 'a constraint 'a = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "z1" contains a cycle:
         the 1st type parameter of "z1" is constrained to "t",
         "t" = "int s z1"
|}];;

type 'a z0 = 'a constraint 'a = 'b z1
and t = int s z0
and 'a s = 'a constraint 'a = t
and 'a z1 = 'a constraint 'a = 'b s
;;
[%%expect{|
Line 1, characters 0-37:
1 | type 'a z0 = 'a constraint 'a = 'b z1
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "z0" contains a cycle:
         the 1st type parameter of "z0" is constrained to "t s z1",
         "t s z1" = "t s",
         "t s" = "t",
         "t" = "int s z0"
|}];;

type 'a t constraint 'a = 'b t
[%%expect{|
Line 1, characters 0-30:
1 | type 'a t constraint 'a = 'b t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t" contains a cycle:
         the 1st type parameter of "t" is constrained to "'a t"
|}];;



(* Examples from the comments in typedecl: *)


(* well-founded *)
type t = Foo of u
and u = t
[%%expect{|
type t = Foo of u
and u = t
|}];;

(* ill-founded *)
type 'a t = Foo of 'a
and u = u t
[%%expect{|
Line 2, characters 0-11:
2 | and u = u t
    ^^^^^^^^^^^
Error: The definition of "u" contains a cycle:
         "u" = "u t",
         "u t" contains "u"
|}];;

(* well-founded *)
type t = < x : 'a > as 'a
[%%expect{|
type t = < x : 'a > as 'a
|}];;

(* ill-founded, unless -rectypes is used *)
type t = (int * 'a) as 'a
[%%expect{|
Line 1, characters 23-25:
1 | type t = (int * 'a) as 'a
                           ^^
Error: This alias is bound to type "int * 'a"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "int * 'a"
|}];;

(* well-founded *)
type t = < x : t >
[%%expect{|
type t = < x : t >
|}];;

(* ill-founded, unless -rectypes is used *)
type t = (int * t)
[%%expect{|
Line 1, characters 0-18:
1 | type t = (int * t)
    ^^^^^^^^^^^^^^^^^^
Error: The definition of "t" contains a cycle:
         "t" = "int * t",
         "int * t" contains "t"
|}];;

(* well-founded *)
module rec M : sig type t = < x : M.t > end = M
[%%expect{|
module rec M : sig type t = < x : M.t > end
|}];;

(* ill-founded, unless -rectypes is used *)
module rec M : sig type t = int * M.t end = M
[%%expect{|
Line 1, characters 0-45:
1 | module rec M : sig type t = int * M.t end = M
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "M.t" contains a cycle:
         "M.t" = "int * M.t",
         "int * M.t" contains "M.t"
|}];;

module type T = sig type t end
module Fix(F:(T -> T)) = struct
  (* this recursive definition is well-founded
          as F(Fixed).t contains no reachable type expression. *)
  module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
end

(* well-founded *)
module M = Fix(functor (M:T) -> struct type t = < x : M.t > end)
[%%expect{|
module type T = sig type t end
module Fix :
  (F : T -> T) -> sig module rec Fixed : sig type t = F(Fixed).t end end
module M : sig module rec Fixed : sig type t = < x : Fixed.t > end end
|}];;

(* ill-founded *)
module M = Fix(functor (M:T) -> struct type t = int * M.t end);;
[%%expect{|
Line 1, characters 11-62:
1 | module M = Fix(functor (M:T) -> struct type t = int * M.t end);;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       The definition of "Fixed.t" contains a cycle:
         "Fixed.t" = "F(Fixed).t",
         "F(Fixed).t" = "int * Fixed.t",
         "int * Fixed.t" contains "Fixed.t"
|}];;
