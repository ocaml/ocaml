(* TEST
   * expect *)

module M = struct type t = A | B end;;
[%%expect{|
module M : sig type t = A | B end
|}];;

type 'a t = I : int t | M : M.t t;;
[%%expect{|
type 'a t = I : int t | M : M.t t
|}];;

type dyn = Sigma : 'a t * 'a -> dyn;;
[%%expect{|
type dyn = Sigma : 'a t * 'a -> dyn
|}];;

let f = function Sigma (M, A) -> ();;
[%%expect{|
Line 1, characters 8-35:
1 | let f = function Sigma (M, A) -> ();;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Sigma (M, B)
val f : dyn -> unit = <fun>
|}];;

type _ t = IntLit : int t | BoolLit : bool t;;
[%%expect{|
type _ t = IntLit : int t | BoolLit : bool t
|}]

(* The following should warn *)

let f (type a) t (x : a) =
  ignore  (t : a t);
  match t, x with
  | IntLit, n -> n+1
  | BoolLit, b -> 1
;;
[%%expect{|
val f : 'a t -> 'a -> int = <fun>
|}, Principal{|
Line 4, characters 4-10:
4 |   | IntLit, n -> n+1
        ^^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and a as equal.
But the knowledge of these types is not principal.
Line 5, characters 4-11:
5 |   | BoolLit, b -> 1
        ^^^^^^^
Warning 18 [not-principal]: typing this pattern requires considering bool and a as equal.
But the knowledge of these types is not principal.
val f : 'a t -> 'a -> int = <fun>
|}]

let f (type a) t (x : a) =
  ignore  (t : a t);
  match t, x with
  | IntLit, n -> n+1
  | _, _ -> 1
;;
[%%expect{|
val f : 'a t -> 'a -> int = <fun>
|}, Principal{|
Line 4, characters 4-10:
4 |   | IntLit, n -> n+1
        ^^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and a as equal.
But the knowledge of these types is not principal.
val f : 'a t -> 'a -> int = <fun>
|}]


let f (type a) t (x : a) =
  begin match t, x with
  | IntLit, n -> n+1
  | BoolLit, b -> 1
  end;
  ignore  (t : a t)
;;
[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit, b -> 1
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let f (type a) t (x : a) =
  begin match t, x with
  | IntLit, n -> n+1
  | _, _ -> 1
  end;
  ignore  (t : a t)
;;
[%%expect{|
Line 3, characters 17-18:
3 |   | IntLit, n -> n+1
                     ^
Error: This expression has type a but an expression was expected of type int
|}]

(**********************)
(* Derived from #9019 *)
(**********************)

type _ ab = A | B

module M : sig
  type _ mab
  type _ t = AB : unit ab t | MAB : unit mab t
end = struct
  type 'a mab = 'a ab = A | B
  type _ t = AB : unit ab t | MAB : unit mab t
end;;
[%%expect{|
type _ ab = A | B
module M : sig type _ mab type _ t = AB : unit ab t | MAB : unit mab t end
|}]

open M;;
[%%expect{|
|}]

let f1 t1 =
  match t1 with
  | AB -> true
  | MAB -> false;;
[%%expect{|
val f1 : unit ab M.t -> bool = <fun>
|}, Principal{|
Line 4, characters 4-7:
4 |   | MAB -> false;;
        ^^^
Warning 18 [not-principal]: typing this pattern requires considering unit M.mab and unit ab as equal.
But the knowledge of these types is not principal.
val f1 : unit ab M.t -> bool = <fun>
|}]

let f2 (type x) t1 =
  ignore (t1 : x t);
  match t1 with
  | AB -> true
  | MAB -> false;;
[%%expect{|
val f2 : 'x M.t -> bool = <fun>
|}, Principal{|
Line 4, characters 4-6:
4 |   | AB -> true
        ^^
Warning 18 [not-principal]: typing this pattern requires considering unit ab and x as equal.
But the knowledge of these types is not principal.
Line 5, characters 4-7:
5 |   | MAB -> false;;
        ^^^
Warning 18 [not-principal]: typing this pattern requires considering unit M.mab and x as equal.
But the knowledge of these types is not principal.
val f2 : 'x M.t -> bool = <fun>
|}]

(* This should warn *)
let f3 t1 =
  ignore (t1 : unit ab t);
  match t1 with
  | AB -> true
  | MAB -> false;;
[%%expect{|
val f3 : unit ab M.t -> bool = <fun>
|}, Principal{|
Line 5, characters 4-7:
5 |   | MAB -> false;;
        ^^^
Warning 18 [not-principal]: typing this pattern requires considering unit M.mab and unit ab as equal.
But the knowledge of these types is not principal.
val f3 : unit ab M.t -> bool = <fun>
|}]

(* Example showing we need to warn when any part of the type is non generic. *)
type (_,_) eq = Refl : ('a,'a) eq;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
|}]

let g1 (type x) (e : (x, int option) eq) (x : x) : int option =
   let Refl = e in x;;
[%%expect{|
val g1 : ('x, int option) eq -> 'x -> int option = <fun>
|}]

(* This should warn *)
let g2 (type x) (e : (x, _ option) eq) (x : x) : int option =
   ignore (e : (x, int option) eq);
   let Refl = e in x;;
[%%expect{|
val g2 : ('x, int option) eq -> 'x -> int option = <fun>
|}, Principal{|
Line 3, characters 7-11:
3 |    let Refl = e in x;;
           ^^^^
Warning 18 [not-principal]: typing this pattern requires considering x and int option as equal.
But the knowledge of these types is not principal.
val g2 : ('x, int option) eq -> 'x -> int option = <fun>
|}]

(* Issues with "principal level" *)

module Foo : sig
  type t
end = struct
  type t = int
end

type _ gadt = F : Foo.t gadt

type  'a t = { a: 'a; b: 'a gadt } ;;
[%%expect{|
module Foo : sig type t end
type _ gadt = F : Foo.t gadt
type 'a t = { a : 'a; b : 'a gadt; }
|}]

let () =
  match [] with
  | [ { a = 3; _ } ; { b = F; _ }] -> ()
  | _ -> ();;
[%%expect{|
|}, Principal{|
Line 3, characters 27-28:
3 |   | [ { a = 3; _ } ; { b = F; _ }] -> ()
                               ^
Warning 18 [not-principal]: typing this pattern requires considering Foo.t and int as equal.
But the knowledge of these types is not principal.
|}]

let () =
  match [] with
  | [ { b = F; _ } ; { a = 3; _ }] -> ()
  | _ -> ();;
[%%expect{|
Line 3, characters 27-28:
3 |   | [ { b = F; _ } ; { a = 3; _ }] -> ()
                               ^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type Foo.t
|}]

type (_, _, _) eq3 = Refl3 : ('a, 'a, 'a) eq3

type  'a t = { a: 'a; b: (int, Foo.t, 'a) eq3 }
;;
[%%expect{|
type (_, _, _) eq3 = Refl3 : ('a, 'a, 'a) eq3
type 'a t = { a : 'a; b : (int, Foo.t, 'a) eq3; }
|}]

let () =
  match [] with
  | [ { a = 3; _ }; { b = Refl3 ; _ }] -> ()
  | _ -> ()
;;
[%%expect{|
|}, Principal{|
Line 3, characters 26-31:
3 |   | [ { a = 3; _ }; { b = Refl3 ; _ }] -> ()
                              ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and Foo.t as equal.
But the knowledge of these types is not principal.
|}]

let () =
  match [] with
  | [ { b = Refl3 ; _ }; { a = 3; _ } ] -> ()
  | _ -> ()
;;
[%%expect{|
|}, Principal{|
Line 3, characters 12-17:
3 |   | [ { b = Refl3 ; _ }; { a = 3; _ } ] -> ()
                ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and Foo.t as equal.
But the knowledge of these types is not principal.
|}]

(* Unify with 'a first *)

type  'a t = { a: 'a; b: ('a, int, Foo.t) eq3 }
;;
[%%expect{|
type 'a t = { a : 'a; b : ('a, int, Foo.t) eq3; }
|}]

let () =
  match [] with
  | [ { a = 3; _ }; { b = Refl3 ; _ }] -> ()
  | _ -> ()
[%%expect{|
|}, Principal{|
Line 3, characters 26-31:
3 |   | [ { a = 3; _ }; { b = Refl3 ; _ }] -> ()
                              ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and Foo.t as equal.
But the knowledge of these types is not principal.
|}]

let () =
  match [] with
  | [ { b = Refl3 ; _ }; { a = 3; _ } ] -> ()
  | _ -> ()
[%%expect{|
|}, Principal{|
Line 3, characters 12-17:
3 |   | [ { b = Refl3 ; _ }; { a = 3; _ } ] -> ()
                ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering int and Foo.t as equal.
But the knowledge of these types is not principal.
|}]


(*************)
(* Some more *)
(*************)

module M : sig type t end = struct type t = int end
module N : sig type t end = struct type t = int end
;;
[%%expect{|
module M : sig type t end
module N : sig type t end
|}]

type 'a foo = { x : 'a; eq : (M.t, N.t, 'a) eq3 };;
[%%expect{|
type 'a foo = { x : 'a; eq : (M.t, N.t, 'a) eq3; }
|}]

let foo x =
  match x with
  | { x = x; eq = Refl3 } -> x
;;
[%%expect{|
val foo : M.t foo -> M.t = <fun>
|}, Principal{|
Line 3, characters 18-23:
3 |   | { x = x; eq = Refl3 } -> x
                      ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering M.t and N.t as equal.
But the knowledge of these types is not principal.
val foo : M.t foo -> M.t = <fun>
|}]

let foo x =
  match x with
  | { x = (x : int); eq = Refl3 } -> x
;;
[%%expect{|
val foo : int foo -> int = <fun>
|}, Principal{|
Line 3, characters 26-31:
3 |   | { x = (x : int); eq = Refl3 } -> x
                              ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering M.t and N.t as equal.
But the knowledge of these types is not principal.
val foo : int foo -> int = <fun>
|}]

let foo x =
  match x with
  | { x = (x : N.t); eq = Refl3 } -> x
;;
[%%expect{|
Line 3, characters 4-33:
3 |   | { x = (x : N.t); eq = Refl3 } -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type N.t foo
       but a pattern was expected which matches values of type 'a
       This instance of M.t is ambiguous:
       it would escape the scope of its equation
|}, Principal{|
Line 3, characters 26-31:
3 |   | { x = (x : N.t); eq = Refl3 } -> x
                              ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering M.t and N.t as equal.
But the knowledge of these types is not principal.
Line 3, characters 4-33:
3 |   | { x = (x : N.t); eq = Refl3 } -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type N.t foo
       but a pattern was expected which matches values of type 'a
       This instance of M.t is ambiguous:
       it would escape the scope of its equation
|}]

let foo x =
  match x with
  | { x = (x : string); eq = Refl3 } -> x
;;
[%%expect{|
val foo : string foo -> string = <fun>
|}, Principal{|
Line 3, characters 29-34:
3 |   | { x = (x : string); eq = Refl3 } -> x
                                 ^^^^^
Warning 18 [not-principal]: typing this pattern requires considering M.t and N.t as equal.
But the knowledge of these types is not principal.
val foo : string foo -> string = <fun>
|}]

let bar x =
  match x with
  | { x = x; _ } -> x
;;
[%%expect{|
val bar : 'a foo -> 'a = <fun>
|}]

let bar x =
  match x with
  | { x = (x : int); _ } -> x
;;
[%%expect{|
val bar : int foo -> int = <fun>
|}]

let bar x =
  match x with
  | { x = (x : N.t); _ } -> x
;;
[%%expect{|
val bar : N.t foo -> N.t = <fun>
|}]

let bar x =
  match x with
  | { x = (x : string); _ } -> x
;;
[%%expect{|
val bar : string foo -> string = <fun>
|}]

(* #10822 *)
type t
type u = private t
type ('a, 'b) eq = Refl : ('a, 'a) eq
[%%expect{|
type t
type u = private t
type ('a, 'b) eq = Refl : ('a, 'a) eq
|}]

let foo (type s) x (Refl : (s, u) eq) =
  (x : s :> t)
[%%expect{|
val foo : 's -> ('s, u) eq -> t = <fun>
|}]
