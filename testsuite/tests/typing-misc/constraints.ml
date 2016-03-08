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
val f : 'a -> unit = <fun>
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
end
;; (* fails *)
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

module PR6505a = struct
  type 'o is_an_object = < .. > as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let y : ('o, 'o) abs = object end
end;; 
let _ = PR6505a.y#bang;; (* fails *)
[%%expect{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val y : (<  > is_an_object, <  > is_an_object) abs
  end
Line _, characters 8-17:
Error: This expression has type
         (<  > PR6505a.is_an_object, <  > PR6505a.is_an_object) PR6505a.abs
       It has no method bang
|}, Principal{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val y : (<  >, <  >) abs
  end
Line _, characters 8-17:
Error: This expression has type (<  >, <  >) PR6505a.abs
       It has no method bang
|}]

module PR6505b = struct
  type 'o is_an_object = [> ] as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let x : ('a, 'a) abs = `Foo 6
end;;
let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
[%%expect{|
module PR6505b :
  sig
    type 'o is_an_object = 'o constraint 'o = [>  ]
    and ('a, 'l) abs = 'l constraint 'a = 'l is_an_object
    val x : (([> `Foo of int ] as 'a) is_an_object, 'a is_an_object) abs
  end
Line _, characters 23-57:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`Foo _
Exception: Match_failure ("", 6, 23).
|}]
