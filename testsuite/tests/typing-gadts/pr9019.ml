(* TEST
 expect;
*)

(* #9012 by Thomas Refis *)

type ab = A | B

module M : sig
  type mab = A | B
  type _ t = AB : ab t | MAB : mab t
  val ab : mab t
end = struct
  type mab = ab = A | B
  type _ t = AB : ab t | MAB : mab t
  let ab = AB
end
[%%expect{|
type ab = A | B
module M :
  sig type mab = A | B type _ t = AB : ab t | MAB : mab t val ab : mab t end
|}]

open M

let f (type x) (t1 : x t) (t2 : x t) (x : x) =
  match t1, t2, x with
  | AB,  AB, A -> 1
  | MAB, _, A -> 2
  | _,  AB, B -> 3
  | _, MAB, B -> 4
[%%expect{|
Lines 4-8, characters 2-18:
4 | ..match t1, t2, x with
5 |   | AB,  AB, A -> 1
6 |   | MAB, _, A -> 2
7 |   | _,  AB, B -> 3
8 |   | _, MAB, B -> 4
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(AB, MAB, A)"

val f : 'x M.t -> 'x M.t -> 'x -> int = <fun>
|}]

let () = ignore (f M.ab MAB A)
[%%expect{|
Exception: Match_failure ("", 4, 2).
|}]

(* variant *)

type _ ab = A | B

module M : sig
  type _ mab
  type _ t = AB : unit ab t | MAB : unit mab t
  val ab : unit mab t
  val a : 'a mab
  val b : 'a mab
end = struct
  type 'a mab = 'a ab = A | B
  type _ t = AB : unit ab t | MAB : unit mab t
  let ab = AB
  let a = A
  let b = B
end;;
[%%expect{|
type _ ab = A | B
module M :
  sig
    type _ mab
    type _ t = AB : unit ab t | MAB : unit mab t
    val ab : unit mab t
    val a : 'a mab
    val b : 'a mab
  end
|}]

open M

(* The second clause isn't redundant *)
let f (type x) (t1 : x t) (t2 : x t) (x : x) =
  match t1, t2, x with
  | AB,  AB, A -> 1
  | _, AB, A -> 2
  | _, AB, B -> 3
  | _, MAB, _ -> 4;;
[%%expect{|
val f : 'x M.t -> 'x M.t -> 'x -> int = <fun>
|}]

(* the answer shouldn't be 3 *)
let x = f MAB M.ab M.a;;
[%%expect{|
val x : int = 2
|}]

(* using records *)

type ab = { a : int }

module M : sig
  type mab = { a : int }

  type _ t = AB : ab t | MAB : mab t

  val a : mab
  val ab : mab t
end = struct
  type mab = ab = { a : int }

  type _ t = AB : ab t | MAB : mab t

  let a = { a = 42 }
  let ab = AB
end;;
[%%expect{|
type ab = { a : int; }
module M :
  sig
    type mab = { a : int; }
    type _ t = AB : ab t | MAB : mab t
    val a : mab
    val ab : mab t
  end
|}]

open M

let f (type x) (t1 : x t) (t2 : x t) (x : x) =
  match t1, t2, x with
  | AB,  AB, { a = _ } -> 1
  | MAB, _,  { a = _ } -> 2
  | _,  AB,  { a = _ } -> 3
  | _, MAB,  { a = _ } -> 4;;
[%%expect{|
Line 7, characters 4-22:
7 |   | _,  AB,  { a = _ } -> 3
        ^^^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val f : 'x M.t -> 'x M.t -> 'x -> int = <fun>
|}]

let p = f M.ab MAB { a = 42 };;
[%%expect{|
val p : int = 4
|}]


(* #9019 by Leo White *)

type _ a_or_b =
  A_or_B : [< `A of string | `B of int] a_or_b

type _ a =
  | A : [> `A of string] a
  | Not_A : _ a

let f (type x) (a : x a) (a_or_b : x a_or_b) (x : x) =
  match a, a_or_b, x with
  | Not_A, A_or_B, `B i -> print_int i
  | _, A_or_B, `A s -> print_string s
[%%expect{|
type _ a_or_b = A_or_B : [< `A of string | `B of int ] a_or_b
type _ a = A : [> `A of string ] a | Not_A : 'a a
Lines 9-11, characters 2-37:
 9 | ..match a, a_or_b, x with
10 |   | Not_A, A_or_B, `B i -> print_int i
11 |   | _, A_or_B, `A s -> print_string s
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(A, A_or_B, `B _)"

val f : 'x a -> 'x a_or_b -> 'x -> unit = <fun>
|}]

let segfault = f A A_or_B (`B 0)
[%%expect{|
Exception: Match_failure ("", 9, 2).
|}]


(* Another example *)
type (_, _) b =
  | A : ([< `A ], 'a) b
  | B : ([< `B of 'a], 'a) b

type _ ty =
  | String_option : string option ty

let f (type x) (type y) (b : (x, y ty) b) (x : x) (y : y) =
  match b, x, y with
  | B, `B String_option, Some s -> print_string s
  | A, `A, _ -> ()
[%%expect{|
type (_, _) b = A : ([< `A ], 'a) b | B : ([< `B of 'a ], 'a) b
type _ ty = String_option : string option ty
Lines 9-11, characters 2-18:
 9 | ..match b, x, y with
10 |   | B, `B String_option, Some s -> print_string s
11 |   | A, `A, _ -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
    "(B, `B String_option, None)"

val f : ('x, 'y ty) b -> 'x -> 'y -> unit = <fun>
|}]

let segfault = f B (`B String_option) None
[%%expect{|
Exception: Match_failure ("", 9, 2).
|}]

(* More polymorphic variants *)

type 'a a = private [< `A of 'a];;
let f (x : _ a) = match x with `A None -> ();;
[%%expect{|
type 'a a = private [< `A of 'a ]
Line 2, characters 18-44:
2 | let f (x : _ a) = match x with `A None -> ();;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "`A (Some _)"

val f : 'a option a -> unit = <fun>
|}]

let f (x : [> `A] a) = match x with `A `B -> ();;
[%%expect{|
Line 1, characters 23-47:
1 | let f (x : [> `A] a) = match x with `A `B -> ();;
                           ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "`A `A"

val f : [< `A | `B > `A ] a -> unit = <fun>
|}]
