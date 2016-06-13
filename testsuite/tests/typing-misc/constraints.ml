type 'a t = [`A of 'a t t] as 'a;; (* fails *)
[%%expect{|
Line _, characters 12-32:
Error: Constraints are not satisfied in this type.
       Type
       [ `A of 'a ] t t as 'a
       should be an instance of
       ([ `A of 'b t t ] as 'b) t
|}, Principal{|
type 'a t = [ `A of 'b t t ] as 'b constraint 'a = [ `A of 'a t t ]
|}];;
type 'a t = [`A of 'a t t];; (* fails *)
[%%expect{|
Line _, characters 0-26:
Error: In the definition of t, type 'a t t should be 'a t
|}];;
type 'a t = [`A of 'a t t] constraint 'a = 'a t;;
[%%expect{|
type 'a t = [ `A of 'a t t ] constraint 'a = 'a t
|}];;
type 'a t = [`A of 'a t] constraint 'a = 'a t;;
[%%expect{|
type 'a t = [ `A of 'a t ] constraint 'a = 'a t
|}];;
type 'a t = [`A of 'a] as 'a;;
[%%expect{|
type 'a t = 'a constraint 'a = [ `A of 'a ]
|}, Principal{|
type 'a t = [ `A of 'b ] as 'b constraint 'a = [ `A of 'a ]
|}];;
type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)
[%%expect{|
Line _, characters 42-51:
Error: The type abbreviation t is cyclic
|}];;

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* fails *)
[%%expect{|
type 'a t = 'a
Line _, characters 11-21:
Error: This alias is bound to type 'a t = 'a
       but is used as an instance of type 'a
       The type variable 'a occurs inside 'a
|}];;

let f (x : 'a t) (y : 'a) = x = y;;
[%%expect{|
val f : 'a t -> 'a -> bool = <fun>
|}];;

(* PR#6505 *)
module type PR6505 = sig
  type 'o is_an_object = < .. > as 'o
  and 'o abs constraint 'o = 'o is_an_object
  val abs : 'o is_an_object -> 'o abs
  val unabs : 'o abs -> 'o
end;; (* fails *)
[%%expect{|
Line _, characters 2-44:
Error: The definition of abs contains a cycle:
       'a is_an_object as 'a
|}, Principal{|
module type PR6505 =
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and 'a abs constraint 'a = 'a is_an_object
    val abs : ('a is_an_object as 'a) is_an_object -> 'a abs
    val unabs : ('a is_an_object as 'a) abs -> 'a
  end
|}];;
