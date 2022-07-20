(* TEST
   * expect
*)

type 'a t = [`A of 'a t t] as 'a;; (* fails *)
[%%expect{|
Line 1, characters 0-32:
1 | type 'a t = [`A of 'a t t] as 'a;; (* fails *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation t is cyclic
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
Error: The type abbreviation t is cyclic
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
Line 3, characters 2-44:
3 |   and 'o abs constraint 'o = 'o is_an_object
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of abs contains a cycle:
       'a is_an_object as 'a
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
  type ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let y : ('o, 'o) abs = object end
end;;
let _ = PR6505a.y#bang;; (* fails *)
[%%expect{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    type ('a, 'b) abs = 'b constraint 'a = 'b is_an_object
      constraint 'b = < .. >
    val y : (<  > is_an_object, <  > is_an_object) abs
  end
Line 6, characters 8-17:
6 | let _ = PR6505a.y#bang;; (* fails *)
            ^^^^^^^^^
Error: This expression has type
         (<  > PR6505a.is_an_object, <  > PR6505a.is_an_object) PR6505a.abs
       It has no method bang
|}, Principal{|
module PR6505a :
  sig
    type 'o is_an_object = 'o constraint 'o = < .. >
    type ('a, 'b) abs = 'b constraint 'a = 'b is_an_object
      constraint 'b = < .. >
    val y : (<  >, <  >) abs
  end
Line 6, characters 8-17:
6 | let _ = PR6505a.y#bang;; (* fails *)
            ^^^^^^^^^
Error: This expression has type (<  >, <  >) PR6505a.abs
       It has no method bang
|}]

module PR6505b = struct
  type 'o is_an_object = [> ] as 'o
  type ('k,'l) abs = 'l constraint 'k = 'l is_an_object
  let x : ('a, 'a) abs = `Foo 6
end;;
let () = print_endline (match PR6505b.x with `Bar s -> s);; (* fails *)
[%%expect{|
module PR6505b :
  sig
    type 'o is_an_object = 'o constraint 'o = [>  ]
    type ('a, 'o) abs = 'o constraint 'a = 'o is_an_object
      constraint 'o = [>  ]
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

(* #9866, #9873 *)

type 'a t = 'b  constraint 'a = 'b t;;
[%%expect{|
Line 1, characters 0-36:
1 | type 'a t = 'b  constraint 'a = 'b t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type 'b t t
       but it is used as
         'b t.
       All uses need to match the definition for the recursive type to be regular.
|}]

type 'a t = 'b constraint 'a = ('b * 'b) t;;
[%%expect{|
Line 1, characters 0-42:
1 | type 'a t = 'b constraint 'a = ('b * 'b) t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type ('b * 'b) t t
       but it is used as
         ('b * 'b) t.
       All uses need to match the definition for the recursive type to be regular.
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
Line 1, characters 0-49:
1 | type 'a t = <a : 'a; b : 'b> constraint 'a = 'b t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type 'b t t
       but it is used as
         'b t.
       All uses need to match the definition for the recursive type to be regular.
|}]

type 'a t = <a : 'a; b : 'b> constraint <a : 'a; ..> = 'b t;;
[%%expect{|
Line 1, characters 0-59:
1 | type 'a t = <a : 'a; b : 'b> constraint <a : 'a; ..> = 'b t;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
       In method b: 'b the variable 'b is unbound
|}]

module rec M : sig type 'a t = 'b constraint 'a = 'b t end = M;;
[%%expect{|
Line 1, characters 19-54:
1 | module rec M : sig type 'a t = 'b constraint 'a = 'b t end = M;;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type 'b t t
       but it is used as
         'b t.
       All uses need to match the definition for the recursive type to be regular.
|}]
module rec M : sig type 'a t = 'b constraint 'a = ('b * 'b) t end = M;;
[%%expect{|
Line 1, characters 19-61:
1 | module rec M : sig type 'a t = 'b constraint 'a = ('b * 'b) t end = M;;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor t is defined as
         type ('b * 'b) t t
       but it is used as
         ('b * 'b) t.
       All uses need to match the definition for the recursive type to be regular.
|}]

module type S =
sig
  type !'a s
  type !'a t = 'b  constraint 'a = 'b s
end
[%%expect{|
module type S = sig type !'a s type 'a t = 'b constraint 'a = 'b s end
|}]

(* This still causes a stack overflow *)
(*
module rec M : S =
struct
  type !'a s = 'a M.t
  type !'a t = 'b constraint 'a = 'b s
end
*)

type 'a t = T
  constraint 'a = int
  constraint 'a = float
[%%expect{|
Line 3, characters 13-23:
3 |   constraint 'a = float
                 ^^^^^^^^^^
Error: The type constraints are not consistent.
       Type int is not compatible with type float
|}]

type ('a,'b) t = T
  constraint 'a = int -> float
  constraint 'b = bool -> char
  constraint 'a = 'b
[%%expect{|
Line 4, characters 13-20:
4 |   constraint 'a = 'b
                 ^^^^^^^
Error: The type constraints are not consistent.
       Type int -> float is not compatible with type bool -> char
       Type int is not compatible with type bool
|}]

class type ['a, 'b] a = object
  constraint 'a = 'b
  constraint 'a = int * int
  constraint 'b = float * float
end;;
[%%expect{|
Line 4, characters 2-31:
4 |   constraint 'b = float * float
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class constraints are not consistent.
       Type int * int is not compatible with type float * float
       Type int is not compatible with type float
|}]

(* #11101 *)
type ('node,'self) extension = < node: 'node; self: 'self > as 'self
type 'ext node = < > constraint 'ext = ('ext node, 'self) extension;;
[%%expect{|
type ('node, 'a) extension = 'a constraint 'a = < node : 'node; self : 'a >
type 'a node = <  >
  constraint 'a = ('a node, < node : 'a node; self : 'b > as 'b) extension
|}, Principal{|
type ('node, 'a) extension = < node : 'node; self : 'b > as 'b
  constraint 'a = < node : 'node; self : 'a >
type 'a node = <  >
  constraint 'a = ('a node, < node : 'a node; self : 'b > as 'b) extension
|}]

class type ['node] extension =
  object ('self)
    method clone : 'self
    method node : 'node
  end
type 'ext node = < >
  constraint 'ext = 'ext node #extension ;;
[%%expect{|
class type ['node] extension =
  object ('a) method clone : 'a method node : 'node end
type 'a node = <  > constraint 'a = < clone : 'a; node : 'a node; .. >
|}]

module Raise: sig val default_extension: 'a node extension as 'a end = struct
  let default_extension = failwith "Default_extension failure"
end;;
[%%expect{|
Exception: Failure "Default_extension failure".
|}]
