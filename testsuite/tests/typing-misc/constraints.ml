(* TEST
   * expect
*)

type 'a t = [`A of 'a t t] as 'a;; (* fails *)
[%%expect{|
Line 1, characters 0-32:
1 | type 'a t = [`A of 'a t t] as 'a;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       'a t t as 'a
|}, Principal{|
Line 1, characters 0-32:
1 | type 'a t = [`A of 'a t t] as 'a;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       [ `A of 'a t t ] as 'a
|}];;
type 'a t = [`A of 'a t t];; (* fails *)
[%%expect{|
Line 1, characters 0-26:
1 | type 'a t = [`A of 'a t t];; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type 'a t
       but it is used as
         'a t t.
       All uses need to match the definition for the recursive type to be regular.
|}];;
type 'a t = [`A of 'a t t] constraint 'a = 'a t;; (* fails since 4.04 *)
[%%expect{|
Line 1, characters 0-47:
1 | type 'a t = [`A of 'a t t] constraint 'a = 'a t;; (* fails since 4.04 *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of t contains a cycle:
       [ `A of 'a t/2 t/2 t/2 t/2 ]
|}];;
type 'a t = [`A of 'a t] constraint 'a = 'a t;; (* fails since 4.04 *)
[%%expect{|
Line 1, characters 0-45:
1 | type 'a t = [`A of 'a t] constraint 'a = 'a t;; (* fails since 4.04 *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}];;
type 'a t = [`A of 'a] as 'a;;
[%%expect{|
type 'a t = 'a constraint 'a = [ `A of 'a ]
|}, Principal{|
type 'a t = [ `A of 'b ] as 'b constraint 'a = [ `A of 'a ]
|}];;
type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)
[%%expect{|
Line 1, characters 0-41:
1 | type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of v contains a cycle:
       t
|}];;

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* ok *)
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
Line 3, characters 6-8:
3 |   and 'o abs constraint 'o = 'o is_an_object
          ^^
Error: Constraints are not satisfied in this type.
       Type 'a is_an_object should be an instance of < .. > is_an_object
|}];;

module PR6505a_old = struct
  type 'o is_an_object = < .. > as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let y : ('o, 'o) abs = object end
end;;
[%%expect{|
Line 3, characters 7-9:
3 |   and ('k,'l) abs = 'l constraint 'k = 'l is_an_object
           ^^
Error: Constraints are not satisfied in this type.
       Type 'l is_an_object should be an instance of < .. > is_an_object
|}]

module PR6505a = struct
  type 'o is_an_object = < .. > as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object constraint 'l = < .. >
  let y : ('o, 'o) abs = object end
end;;
let _ = PR6505a.y#bang;; (* fails *)
[%%expect{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    and ('a, 'b) abs = 'b constraint 'a = 'b is_an_object
      constraint 'b = < .. >
    val y : (<  > is_an_object, <  > is_an_object) abs
  end
Line 6, characters 8-17:
6 | let _ = PR6505a.y#bang;; (* fails *)
            ^^^^^^^^^
Error: This expression has type
         (<  > PR6505a.is_an_object, <  > PR6505a.is_an_object) PR6505a.abs
       It has no method bang
|}]

module PR6505b = struct
  type 'o is_an_object = [> ] as 'o
  and ('k,'l) abs = 'l constraint 'k = 'l is_an_object constraint 'l = [> ]
  let x : ('a, 'a) abs = `Foo 6
end;;
let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
[%%expect{|
module PR6505b :
  sig
    type 'o is_an_object = 'o constraint 'o = [>  ]
    and ('a, 'b) abs = 'b constraint 'a = 'b is_an_object
      constraint 'b = [>  ]
    val x : (([> `Foo of int ] as 'a) is_an_object, 'a is_an_object) abs
  end
Line 6, characters 23-57:
6 | let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`Foo _
Exception: Match_failure ("", 6, 23).
|}]


(* #9866 *)

type 'a t = 'b  constraint 'a = 'b t;;
[%%expect{|
type 'a t = 'b constraint 'a = 'b t
|}]

let x : _ t = 3;;
let y : _ t = x;;
let z : _ t = y;;
let u : _ t = z;;
[%%expect{|
val x : int t t t t = 3
val y : int t t t t t t t t = 3
val z : int t t t t t t t t t t t t = 3
val u : int t t t t t t t t t t t t t t t t = 3
|}]

let f x = (x : _ t :> _ t);;
f 5 + 3;;
f 5 = true;;
[%%expect{|
val f : 'a t t t t -> 'a t t = <fun>
- : int = 8
Line 3, characters 6-10:
3 | f 5 = true;;
          ^^^^
Error: This expression has type bool but an expression was expected of type
         int t t = int
|}]

type 'a t = 'a * 'b constraint _ * 'a = 'b t;;
type 'a t = 'a * 'b constraint 'a = 'b t;;
[%%expect{|
type 'b t = 'b * 'b
Line 2, characters 0-40:
2 | type 'a t = 'a * 'b constraint 'a = 'b t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
|}]

type 'a t = <a : 'a; b : 'b> constraint 'a = 'b t;;
[%%expect{|
Line 1, characters 5-7:
1 | type 'a t = <a : 'a; b : 'b> constraint 'a = 'b t;;
         ^^
Error: Constraints are not satisfied in this type.
       Type 'b t should be an instance of 'a t t
|}]

type 'a t = <a : 'a; b : 'b> constraint <a : 'a; ..> = 'b t;;
[%%expect{|
Line 1, characters 0-59:
1 | type 'a t = <a : 'a; b : 'b> constraint <a : 'a; ..> = 'b t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
In method b: 'b the variable 'b is unbound
|}]
