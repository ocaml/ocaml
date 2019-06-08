(* TEST
   * expect
*)

(* Type declarations using type functors. *)

type 'a t = 'a ('a. 'a -> 'a)
;;
[%%expect {|
type 'a t = 'a ('a0. 'a0 -> 'a0)
|}];;

type ('a, 'b) u = ('a, 'b) ('c 'd. int -> int)
;;
[%%expect {|
type ('a, 'b) u = ('a, 'b) ('c 'd. int -> int)
|}];;

let f (g : 'a t) (x : int) = g x
;;
[%%expect {|
val f : int t -> int -> int = <fun>
|}];;

let g (f : (bool, unit) u) (x : int) = f x
;;
[%%expect {|
val g : (bool, unit) u -> int -> int = <fun>
|}];;





(* Type constraint. *)

type 'a constrained = 'b constraint 'b = 'a ('c. 'c)
;;
[%%expect {|
type 'a constrained = 'a ('c. 'c)
|}];;

type ('a, 'b) eq = unit constraint 'a ('c. 'c) = 'b ('c. 'c)
;;
[%%expect {|
type ('a, 'b) eq = unit constraint 'a = 'b ('c. 'c)
|}];;

let check (eq : (int, int) eq) : unit = eq
;;
[%%expect {|
val check : (int, int) eq -> unit = <fun>
|}];;

(* Expected failure in constraint. *)
let check (not_eq : (int, bool) eq) : unit = not_eq
;;
[%%expect {|
Line 1, characters 26-30:
1 | let check (not_eq : (int, bool) eq) : unit = not_eq
                              ^^^^
Error: This type bool should be an instance of type int
|}];;

(* Expected failure in generalising constraints. *)
let check (eq : (int, int) ('a 'b. ('a, 'b) eq)) : unit = eq
;;
[%%expect {|
Line 1, characters 28-46:
1 | let check (eq : (int, int) ('a 'b. ('a, 'b) eq)) : unit = eq
                                ^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized: it is bound to
       'b ('c. 'c).
|}];;




(* Type declarations referencing variants. *)

type 'a variant = A | B of 'a ('a. 'a)
;;
[%%expect {|
type 'a variant = A | B of 'a ('a0. 'a0)
|}];;

type t_variant = int ('a. 'a variant)
;;
[%%expect {|
type t_variant = int ('a. 'a variant)
|}];;

let f (x : t_variant) =
  match x with
  | A -> 1
  | B x -> x
;;
[%%expect {|
val f : t_variant -> int ('a. 'a) = <fun>
|}];;

let a : int ('a. 'a) variant = A
;;
[%%expect {|
val a : int ('a. 'a) variant = A
|}];;

let b = B a
;;
[%%expect {|
val b : int ('a. 'a) variant variant = B A
|}];;

let b' = B ((15, ()) : (unit, int) ('b 'a. 'a * 'b))
;;
[%%expect {|
val b' : (unit, int) ('b 'a. 'a * 'b) variant = B (15, ())
|}];;

(* Expected failure in return type. *)
let f (x : t_variant) =
  match x with
  | A -> true
  | B x -> x
;;
[%%expect {|
Line 4, characters 11-12:
4 |   | B x -> x
               ^
Error: This expression has type int ('a. 'a) = int
       but an expression was expected of type bool
|}];;




(* Type functors in record fields. *)

type record = {
    a: 'a 'b. 'a ('c. 'c -> 'c) -> 'b;
    b: int ('c. 'c -> 'c)
  }
;;
[%%expect {|
type record = { a : 'a 'b. 'a ('c. 'c -> 'c) -> 'b; b : int ('c. 'c -> 'c); }
|}];;

let f (x : record) = x.a (fun (x : int) -> x)
;;
[%%expect {|
val f : record -> 'a = <fun>
|}];;

let g (x : record) : int = x.b 15
;;
[%%expect {|
val g : record -> int = <fun>
|}];;

(* Expected failure in return type. *)
let h (x : record) : bool = x.b 15
;;
[%%expect {|
Line 1, characters 28-34:
1 | let h (x : record) : bool = x.b 15
                                ^^^^^^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

(* Expected failure in argument type. *)
let i (x : record) = x.b A
;;
[%%expect {|
Line 1, characters 25-26:
1 | let i (x : record) = x.b A
                             ^
Error: This expression has type 'a variant
       but an expression was expected of type int
|}];;




(* Naked type functors. *)

let w (a : int ('a. 'a)) : int = a
;;
[%%expect {|
val w : int ('a. 'a) -> int = <fun>
|}];;

let x (f : bool ('a. 'a -> 'a)) : bool = f true
;;
[%%expect {|
val x : bool ('a. 'a -> 'a) -> bool = <fun>
|}];;

let y : int ('a. 'a) = 15
;;
[%%expect {|
val y : int ('a. 'a) = 15
|}];;

(* Expected failure in return type. *)
let z : int ('a. 'a) = true
;;
[%%expect {|
Line 1, characters 23-27:
1 | let z : int ('a. 'a) = true
                           ^^^^
Error: This expression has type bool but an expression was expected of type
         int ('a. 'a) = int
|}];;




(* Functors as type parameters. *)

let a : float ('a. 'a) variant = A
;;
[%%expect {|
val a : float ('a. 'a) variant = A
|}];;

let b : float ('a. 'a) variant = B 15.0
;;
[%%expect {|
val b : float ('a. 'a) variant = B 15.
|}];;

let c : int ('a. float) variant = B 15.0
;;
[%%expect {|
val c : int ('a. float) variant = B 15.
|}];;

(* Expected failure in GADT type. *)
let d : int ('a. int) variant = B 15.0
;;
[%%expect {|
Line 1, characters 34-38:
1 | let d : int ('a. int) variant = B 15.0
                                      ^^^^
Error: This expression has type float but an expression was expected of type
         int ('a. int) ('a. 'a) = int
|}];;




(* Functors using newtypes. *)

let e (type t) (x : int ('a. t)) (y : t) = x = y
;;
[%%expect {|
val e : int ('a. 't) -> 't -> bool = <fun>
|}];;




(* Functors as module type overrides. *)

module type S = sig
  type t

  val x : t
end
;;

module T = struct
  type t = int

  let x = 15
end
;;
[%%expect {|
module type S = sig type t val x : t end
module T : sig type t = int val x : int end
|}];;

include (T : S with type t := (bool, bool) ('a 'b. int))
;;
[%%expect {|
val x : (bool, bool) ('a 'b. int) = 15
|}];;




(* Omitted parameter error. *)

type missing_param = int ('a. 'b)
;;

[%%expect{|
Line 1, characters 30-32:
1 | type missing_param = int ('a. 'b)
                                  ^^
Error: The type variable 'b is unbound in this type declaration.
|}]




(* Occurrence checks. *)

let f (x : int ('a. 'b)) (y : 'b) = x = y

[%%expect{|
val f : int ('a. 'b) -> 'b -> bool = <fun>
|}]

let f (x : int ('a. 'a * 'b)) (y : 'b) = x = y
;;

[%%expect{|
Line 1, characters 45-46:
1 | let f (x : int ('a. 'a * 'b)) (y : 'b) = x = y
                                                 ^
Error: This expression has type 'b but an expression was expected of type
         int ('a. 'a * 'b) = int * 'b
       The type variable 'b occurs inside int ('a. 'a * 'b)
|}]

let f : 'a 'b. 'a ('a. 'a) -> 'b -> 'a = fun x y -> x
;;

[%%expect{|
val f : 'a ('a0. 'a0) -> 'b -> 'a = <fun>
|}]

let f : 'a 'b. 'a ('a. 'b) -> 'b -> 'b = fun x y -> x
;;

[%%expect{|
val f : 'a ('a0. 'b) -> 'b -> 'b = <fun>
|}]

let f : 'a 'b. int ('a. 'b) -> 'b -> 'b = fun x y -> x
;;
[%%expect{|
val f : int ('a. 'b) -> 'b -> 'b = <fun>
|}]




(* Row types. *)

type static_row = [`A | `B]
;;

type 'a dynamic_row = [> `A | `B] as 'a
;;
[%%expect{|
type static_row = [ `A | `B ]
type 'a dynamic_row = 'a constraint 'a = [> `A | `B ]
|}]

type static_row_functor =
  [ int ('a. [`C])
  | [`D] ('a. 'a)
  | static_row ('a. 'a)
  | bool ('a. static_row dynamic_row) ]
;;
[%%expect{|
type static_row_functor = [ `A | `B | `C | `D ]
|}]

type 'a dynamic_row_passthrough = 'b
  constraint 'a = [> `A | `B]
  constraint 'b = 'a ('c. 'c)
;;
[%%expect{|
type 'a dynamic_row_passthrough = 'a ('c. 'c) constraint 'a = _[> `A | `B ]
|}]

let f (x : 'a dynamic_row_passthrough) : [`A | `B | `C] = x
;;
[%%expect{|
val f : [ `A | `B | `C ] dynamic_row_passthrough -> [ `A | `B | `C ] = <fun>
|}]

let f (x : 'a dynamic_row_passthrough) : 'a ('c. 'c) = x
;;
[%%expect{|
val f : [ `A | `B | `C ] dynamic_row_passthrough -> [ `A | `B | `C ] ('c. 'c) =
  <fun>
|}]

(* Expected failure in non-row variable. *)
type 'a dynamic_row_functor = 'a ('a. [ | 'a])
;;
[%%expect{|
Line 1, characters 42-44:
1 | type 'a dynamic_row_functor = 'a ('a. [ | 'a])
                                              ^^
Error: The type 'a does not expand to a polymorphic variant type
Hint: Did you mean `a?
|}]

(* Expected failure in non-row variable. *)
type 'a dynamic_row_functor = ([>] as 'a) ('a. [ | 'a])
[%%expect{|
Line 1, characters 51-53:
1 | type 'a dynamic_row_functor = ([>] as 'a) ('a. [ | 'a])
                                                       ^^
Error: The type 'a does not expand to a polymorphic variant type
Hint: Did you mean `a?
|}]

(* Expected failure in generalising constraint. *)
type 'a dynamic_row_functor = ([>] as 'a) ('a. [ | ([>] as 'a)])
[%%expect{|
Line 1, characters 52-61:
1 | type 'a dynamic_row_functor = ([>] as 'a) ('a. [ | ([>] as 'a)])
                                                        ^^^^^^^^^
Error: The type [>  ] does not expand to a polymorphic variant type
|}]

(* Expected failure in non-row variable. *)
type static_row_functor_fail = [ | [`B] ('a. [`A | 'a])]
;;
[%%expect{|
Line 1, characters 51-53:
1 | type static_row_functor_fail = [ | [`B] ('a. [`A | 'a])]
                                                       ^^
Error: The type 'a does not expand to a polymorphic variant type
Hint: Did you mean `a?
|}]

(* Expected failure in generalising constraint. *)
type static_row_functor_fail = [ | [`B] ('a. [`A | ([`B] as 'a)])]
;;
[%%expect{|
Line 1, characters 41-64:
1 | type static_row_functor_fail = [ | [`B] ('a. [`A | ([`B] as 'a)])]
                                             ^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized: it is bound to
       [ `B ].
|}]
