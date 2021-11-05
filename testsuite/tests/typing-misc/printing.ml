(* TEST
   * expect
*)

(* PR#7012 *)

type t = [ 'A_name | `Hi ];;
[%%expect{|
Line 1, characters 11-18:
1 | type t = [ 'A_name | `Hi ];;
               ^^^^^^^
Error: The type 'A_name does not expand to a polymorphic variant type
Hint: Did you mean `A_name?
|}];;

let f (x:'id_arg) = x;;
[%%expect{|
val f : 'id_arg -> 'id_arg = <fun>
|}];;

let f (x:'Id_arg) = x;;
[%%expect{|
val f : 'Id_arg -> 'Id_arg = <fun>
|}];;

(* GPR#1204, GPR#1329 *)
type 'a id = 'a
let f (x : [< [`Foo] id]) = ();;
[%%expect{|
type 'a id = 'a
val f : [< [ `Foo ] id ] -> unit = <fun>
|}];;

module M = struct module N = struct type t = [`A] end end;;
let f x = (x :> M.N.t);;
[%%expect{|
module M : sig module N : sig type t = [ `A ] end end
val f : [< M.N.t ] -> M.N.t = <fun>
|}]
module G = M.N;;
let f x = (x :> G.t);;
[%%expect{|
module G = M.N
val f : [< G.t ] -> G.t = <fun>
|}]


(* GPR#2034 *)

type (+' a', -' a'b, 'cd') t = ' a'b -> ' a'  * 'cd';;
[%%expect{|
type (' a', ' a'b, 'cd') t = ' a'b -> ' a' * 'cd'
|}];;


(* #8856: cycles in types expressions could trigger stack overflows
   when printing subpart of error messages *)

type 'a t = private X of 'a
let zeros = object(self) method next = 0, self end
let x = X zeros;;
[%%expect {|
type 'a t = private X of 'a
val zeros : < next : int * 'a > as 'a = <obj>
Line 3, characters 8-15:
3 | let x = X zeros;;
            ^^^^^^^
Error: Cannot create values of the private type (< next : int * 'a > as 'a) t
|}]


type ('a,'b) eq = Refl: ('a,'a) eq
type t = <m : int * 't> as 't
let f (x:t) (type a) (y:a) (witness:(a,t) eq) = match witness with
  | Refl -> if true then x else y
[%%expect {|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type t = < m : int * 'a > as 'a
Line 4, characters 32-33:
4 |   | Refl -> if true then x else y
                                    ^
Error: This expression has type a but an expression was expected of type t
       This instance of < m : int * 'a > as 'a is ambiguous:
       it would escape the scope of its equation
|}]


type t1 = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>
type t2 = <m : 'a. 'a * ('a * 'foo)> as 'foo
let f (x : t1) : t2 = x;;
[%%expect {|
type t1 = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) >
type t2 = < m : 'a. 'a * ('a * 'b) > as 'b
Line 3, characters 22-23:
3 | let f (x : t1) : t2 = x;;
                          ^
Error: This expression has type t1 but an expression was expected of type t2
       The method m has type 'c. 'c * ('a * < m : 'c. 'b >) as 'b,
       but the expected method type was 'a. 'a * ('a * < m : 'a. 'd >) as 'd
       The universal variable 'a would escape its scope
|}]

(* #9739
   Recursive occurrence checks are only done on type variables.
   However, we are not guaranteed to still have a type variable when printing.
*)

let rec foo () = [42]
and bar () =
  let x = foo () in
  x |> List.fold_left max 0 x
[%%expect {|
Line 4, characters 7-29:
4 |   x |> List.fold_left max 0 x
           ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int
       This is not a function; it cannot be applied.
|}]


(* PR#8917
   In nested recursive definitions, we have to remember all recursive items
   under definitions, not just the last one
 *)

module RecMod = struct
  module A= struct end
  module type s = sig
    module rec A: sig type t end
    and B: sig type t = A.t end
  end
end
[%%expect {|
module RecMod :
  sig
    module A : sig end
    module type s =
      sig module rec A : sig type t end and B : sig type t = A.t end end
  end
|}]
