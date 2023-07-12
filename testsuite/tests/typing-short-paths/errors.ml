(* TEST
 flags = " -short-paths ";
 expect;
*)

module M = struct type t = T end

type t = M.t

let x : M.t = S
[%%expect {|
module M : sig type t = T end
type t = M.t
Line 5, characters 14-15:
5 | let x : M.t = S
                  ^
Error: This variant expression is expected to have type "t"
       There is no constructor "S" within type "t"
|}]

module M = struct
  class c = object method foo = 3 end
end

type c = M.c

let () = (new M.c)#bar
[%%expect {|
module M : sig class c : object method foo : int end end
type c = M.c
Line 7, characters 9-18:
7 | let () = (new M.c)#bar
             ^^^^^^^^^
Error: This expression has type "c"
       It has no method "bar"
|}]


(** Known issue: how to print existential types with equality in scope? *)

type _ ty = Char : char ty
type pair = Pair : 'a ty * 'a -> pair

(* In the following function, we would like to see that [x] has type [char],
   rather than an existential type *)
let f = function
  | Pair (Char, x) -> x + 1
[%%expect {|
type _ ty = Char : char ty
type pair = Pair : 'a ty * 'a -> pair
Line 9, characters 22-23:
9 |   | Pair (Char, x) -> x + 1
                          ^
Error: This expression has type "$Pair_'a"
       but an expression was expected of type "int"
|}]

type _ ty = Char : char ty
type pair = Pair : 'a ty * 'a -> pair

(* In the following function, an error report with the existential type is more
   useful than one stating that type [char] would escape the scope *)
let f = function
  | Pair (Char, x) -> if true then x else 'd'
[%%expect {|
type _ ty = Char : char ty
type pair = Pair : 'a ty * 'a -> pair
Line 7, characters 35-36:
7 |   | Pair (Char, x) -> if true then x else 'd'
                                       ^
Error: This expression has type "$Pair_'a"
       but an expression was expected of type "'a"
       This instance of "$Pair_'a" is ambiguous:
       it would escape the scope of its equation
|}]
