(* TEST
   * expect
*)

module Exp =
  struct

    type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Abs : ('a -> 'b) -> ('a -> 'b) t


    let rec eval : type s . s t -> s =
      function
        | IntLit x -> x
        | BoolLit y -> y
        | Pair (x,y) ->
            (eval x,eval y)
        | App (f,a) ->
            (eval f) (eval a)
        | Abs f -> f

    let discern : type a. a t -> _ = function
        IntLit _ -> 1
      | BoolLit _ -> 2
      | Pair _ -> 3
      | App _ -> 4
      | Abs _ -> 5
  end
;;
[%%expect{|
module Exp :
  sig
    type _ t =
        IntLit : int -> int t
      | BoolLit : bool -> bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Abs : ('a -> 'b) -> ('a -> 'b) t
    val eval : 's t -> 's
    val discern : 'a t -> int
  end
|}];;

module List =
  struct
    type zero
    type _ t =
      | Nil : zero t
      | Cons : 'a * 'b t -> ('a * 'b) t
    let head =
      function
        | Cons (a,b) -> a
    let tail =
      function
        | Cons (a,b) -> b
    let rec length : type a . a t -> int =
      function
        | Nil -> 0
        | Cons (a,b) -> length b
  end
;;
[%%expect{|
module List :
  sig
    type zero
    type _ t = Nil : zero t | Cons : 'a * 'b t -> ('a * 'b) t
    val head : ('a * 'b) t -> 'a
    val tail : ('a * 'b) t -> 'b t
    val length : 'a t -> int
  end
|}];;

module Nonexhaustive =
  struct
    type 'a u =
      | C1 : int -> int u
      | C2 : bool -> bool u

    type 'a v =
      | C1 : int -> int v

    let unexhaustive : type s . s u -> s =
      function
        | C2 x -> x


    module M : sig type t type u end =
      struct
        type t = int
        type u = bool
      end
    type 'a t =
      | Foo : M.t -> M.t t
      | Bar : M.u -> M.u t
    let same_type : type s . s t * s t -> bool  =
      function
        | Foo _ , Foo _ -> true
        | Bar _, Bar _ -> true
  end
;;
[%%expect{|
Lines 11-12, characters 6-19:
11 | ......function
12 |         | C2 x -> x
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
C1 _
Lines 24-26, characters 6-30:
24 | ......function
25 |         | Foo _ , Foo _ -> true
26 |         | Bar _, Bar _ -> true
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Foo _, Bar _)
module Nonexhaustive :
  sig
    type 'a u = C1 : int -> int u | C2 : bool -> bool u
    type 'a v = C1 : int -> int v
    val unexhaustive : 's u -> 's
    module M : sig type t type u end
    type 'a t = Foo : M.t -> M.t t | Bar : M.u -> M.u t
    val same_type : 's t * 's t -> bool
  end
|}];;

module Exhaustive =
  struct
    type t = int
    type u = bool
    type 'a v =
      | Foo : t -> t v
      | Bar : u -> u v

    let same_type : type s . s v * s v -> bool  =
      function
        | Foo _ , Foo _ -> true
        | Bar _, Bar _ -> true
  end
;;
[%%expect{|
module Exhaustive :
  sig
    type t = int
    type u = bool
    type 'a v = Foo : t -> t v | Bar : u -> u v
    val same_type : 's v * 's v -> bool
  end
|}];;

module PR6862 = struct
  class c (Some x) = object method x : int = x end
  type _ opt = Just : 'a -> 'a opt | Nothing : 'a opt
  class d (Just x) = object method x : int = x end
end;;
[%%expect{|
Line 2, characters 10-18:
2 |   class c (Some x) = object method x : int = x end
              ^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None
Line 4, characters 10-18:
4 |   class d (Just x) = object method x : int = x end
              ^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Nothing
module PR6862 :
  sig
    class c : int option -> object method x : int end
    type _ opt = Just : 'a -> 'a opt | Nothing : 'a opt
    class d : int opt -> object method x : int end
  end
|}];;

module Exhaustive2 = struct
  type _ t = Int : int t
  let f (x : bool t option) = match x with None -> ()
end;;
[%%expect{|
module Exhaustive2 :
  sig type _ t = Int : int t val f : bool t option -> unit end
|}];;

module PR6220 = struct
  type 'a t = I : int t | F : float t
  let f : int t -> int = function I -> 1
  let g : int t -> int = function I -> 1 | _ -> 2 (* warn *)
end;;
[%%expect{|
Line 4, characters 43-44:
4 |   let g : int t -> int = function I -> 1 | _ -> 2 (* warn *)
                                               ^
Warning 56 [unreachable-case]: this match case is unreachable.
Consider replacing it with a refutation case '<pat> -> .'
module PR6220 :
  sig
    type 'a t = I : int t | F : float t
    val f : int t -> int
    val g : int t -> int
  end
|}];;

module PR6403 = struct
  type (_, _) eq = Refl : ('a, 'a) eq
  type empty = { bottom : 'a . 'a }
  type ('a, 'b) sum = Left of 'a | Right of 'b

  let notequal : ((int, bool) eq, empty) sum -> empty = function
    | Right empty -> empty
end;;
[%%expect{|
module PR6403 :
  sig
    type (_, _) eq = Refl : ('a, 'a) eq
    type empty = { bottom : 'a. 'a; }
    type ('a, 'b) sum = Left of 'a | Right of 'b
    val notequal : ((int, bool) eq, empty) sum -> empty
  end
|}];;

module PR6437 = struct
  type ('a, 'b) ctx =
    | Nil : (unit, unit) ctx
    | Cons : ('a, 'b) ctx -> ('a * unit, 'b * unit) ctx

  type 'a var =
    | O : ('a * unit) var
    | S : 'a var -> ('a * unit) var

  let rec f : type g1 g2. (g1, g2) ctx * g1 var -> g2 var = function
    | Cons g, O -> O
    | Cons g, S n -> S (f (g, n))
    | _ -> .
  (*| Nil, _ -> (assert false) *)  (* warns, but shouldn't *)
end;;
[%%expect{|
module PR6437 :
  sig
    type ('a, 'b) ctx =
        Nil : (unit, unit) ctx
      | Cons : ('a, 'b) ctx -> ('a * unit, 'b * unit) ctx
    type 'a var = O : ('a * unit) var | S : 'a var -> ('a * unit) var
    val f : ('g1, 'g2) ctx * 'g1 var -> 'g2 var
  end
|}];;

module PR6801 = struct
  type _ value =
    | String : string -> string value
    | Float : float -> float value
    | Any

  let print_string_value (x : string value) =
    match x with
    | String s -> print_endline s (* warn : Any *)
end;;
[%%expect{|
Lines 8-9, characters 4-33:
8 | ....match x with
9 |     | String s -> print_endline s.................
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Any
module PR6801 :
  sig
    type _ value =
        String : string -> string value
      | Float : float -> float value
      | Any
    val print_string_value : string value -> unit
  end
|}];;

module Existential_escape =
  struct
    type _ t = C : int -> int t
    type u = D : 'a t -> u
    let eval (D x) = x
  end
;;
[%%expect{|
Line 5, characters 21-22:
5 |     let eval (D x) = x
                         ^
Error: This expression has type $D_'a t
       but an expression was expected of type 'a
       The type constructor $D_'a would escape its scope
|}];;

module Rectype =
  struct
    type (_,_) t = C : ('a,'a) t
    let f : type s. (s, s*s) t -> unit =
      fun C -> () (* here s = s*s! *)
  end
;;
[%%expect{|
module Rectype :
  sig type (_, _) t = C : ('a, 'a) t val f : ('s, 's * 's) t -> unit end
|}];;

module Or_patterns =
  struct
      type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . s t -> unit =
      function
        | (IntLit _ | BoolLit _) -> ()

end
;;
[%%expect{|
module Or_patterns :
  sig
    type _ t = IntLit : int -> int t | BoolLit : bool -> bool t
    val eval : 's t -> unit
  end
|}];;

module Polymorphic_variants =
  struct
      type _ t =
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . [`A] * s t -> unit =
      function
        | `A, IntLit _ -> ()
        | `A, BoolLit _ -> ()
  end
;;
[%%expect{|
module Polymorphic_variants :
  sig
    type _ t = IntLit : int -> int t | BoolLit : bool -> bool t
    val eval : [ `A ] * 's t -> unit
  end
|}];;

module Propagation = struct
  type _ t =
      IntLit : int -> int t
    | BoolLit : bool -> bool t

  let check : type s. s t -> s = function
    | IntLit n -> n
    | BoolLit b -> b

  let check : type s. s t -> s = fun x ->
    let r = match x with
    | IntLit n -> (n : s )
    | BoolLit b -> b
    in r
end
;;
[%%expect{|
module Propagation :
  sig
    type _ t = IntLit : int -> int t | BoolLit : bool -> bool t
    val check : 's t -> 's
  end
|}, Principal{|
Line 13, characters 19-20:
13 |     | BoolLit b -> b
                        ^
Error: This expression has type bool but an expression was expected of type
         s = bool
       This instance of bool is ambiguous:
       it would escape the scope of its equation
|}];;

module Normal_constrs = struct
  type a = A
  type b = B

  let f = function A -> 1 | B -> 2
end;;
[%%expect{|
Line 5, characters 28-29:
5 |   let f = function A -> 1 | B -> 2
                                ^
Error: This variant pattern is expected to have type a
       There is no constructor B within type a
|}];;

module PR6849 = struct
  type 'a t = Foo : int t

  let f : int -> int = function
      Foo -> 5
end;;
[%%expect{|
Line 5, characters 6-9:
5 |       Foo -> 5
          ^^^
Error: This pattern matches values of type 'a t
       but a pattern was expected which matches values of type int
|}];;

type _ t = Int : int t ;;

let ky x y = ignore (x = y); x ;;

let test : type a. a t -> a =
  function Int -> ky (1 : a) 1
;;
[%%expect{|
type _ t = Int : int t
val ky : 'a -> 'a -> 'a = <fun>
val test : 'a t -> 'a = <fun>
|}];;

let test : type a. a t -> _ =
  function Int -> 1       (* ok *)
;;
[%%expect{|
val test : 'a t -> int = <fun>
|}];;

let test : type a. a t -> _ =
  function Int -> ky (1 : a) 1  (* fails *)
;;
[%%expect{|
Line 2, characters 18-30:
2 |   function Int -> ky (1 : a) 1  (* fails *)
                      ^^^^^^^^^^^^
Error: This expression has type a = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky (1 : a) 1  (* fails *)
  in r
;;
[%%expect{|
Line 2, characters 30-42:
2 |   let r = match x with Int -> ky (1 : a) 1  (* fails *)
                                  ^^^^^^^^^^^^
Error: This expression has type a = int
       but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky 1 (1 : a)  (* fails *)
  in r
;;
[%%expect{|
Line 2, characters 30-42:
2 |   let r = match x with Int -> ky 1 (1 : a)  (* fails *)
                                  ^^^^^^^^^^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let test (type a) x =
  let r = match (x : a t) with Int -> ky 1 1
  in r
;;
[%%expect{|
val test : 'a t -> int = <fun>
|}];;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> (1 : a)       (* ok! *)
  in r
;;
[%%expect{|
val test : 'a t -> 'a = <fun>
|}];;

let test : type a. a t -> _ = fun x ->
  let r = match x with Int -> 1       (* ok! *)
  in r
;;
[%%expect{|
val test : 'a t -> int = <fun>
|}];;

let test : type a. a t -> a = fun x ->
  let r : a = match x with Int -> 1
  in r (* ok *)
;;
[%%expect{|
val test : 'a t -> 'a = <fun>
|}];;

let test2 : type a. a t -> a option = fun x ->
  let r = ref None in
  begin match x with Int -> r := Some (1 : a) end;
  !r (* ok *)
;;
[%%expect{|
val test2 : 'a t -> 'a option = <fun>
|}];;

let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1 end;
  !r (* ok *)
;;
[%%expect{|
val test2 : 'a t -> 'a option = <fun>
|}];;

let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* ok (u non-ambiguous) *)
[%%expect{|
val test2 : 'a t -> 'a option = <fun>
|}];;

let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> u := Some 1; r := !u end;
  !u
;; (* fails because u : (int | a) option ref *)
[%%expect{|
Line 4, characters 46-48:
4 |   begin match x with Int -> u := Some 1; r := !u end;
                                                  ^^
Error: This expression has type int option
       but an expression was expected of type a option
       Type int is not compatible with type a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* ok *)
[%%expect{|
val test2 : 'a t -> 'a option = <fun>
|}];;

let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let a =
    let r : a option ref = ref None in
    begin match x with Int -> r := Some 1; u := !r end;
    !u
  in a
;; (* ok *)
[%%expect{|
val test2 : 'a t -> 'a option = <fun>
|}];;

let either = ky
let we_y1x (type a) (x : a) (v : a t) =
  match v with Int -> let y = either 1 x in y
;; (* fail *)
[%%expect{|
val either : 'a -> 'a -> 'a = <fun>
Line 3, characters 44-45:
3 |   match v with Int -> let y = either 1 x in y
                                                ^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

(* Effect of external consraints *)
let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> (y : a) in (* ok *)
  r
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

let f (type a) (x : a t) y =
  let r = match x with Int -> (y : a) in
  ignore (y : a); (* ok *)
  r
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> y in (* ok *)
  r
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

let f (type a) (x : a t) y =
  let r = match x with Int -> y in
  ignore (y : a); (* ok *)
  r
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

let f (type a) (x : a t) (y : a) =
  match x with Int -> y (* returns 'a *)
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

(* Combination with local modules *)

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = a let z = (y : b) end
    in M.z
;;
[%%expect{|
val f : 'a t -> 'a -> 'a = <fun>
|}];;

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = int let z = (y : b) end
    in M.z
;; (* ok *)
[%%expect{|
val f : 'a t -> int -> int = <fun>
|}];;

(* Objects and variants *)

type _ h =
  | Has_m : <m : int> h
  | Has_b : <b : bool> h

let f : type a. a h -> a = function
  | Has_m -> object method m = 1 end
  | Has_b -> object method b = true end
;;
[%%expect{|
type _ h = Has_m : < m : int > h | Has_b : < b : bool > h
val f : 'a h -> 'a = <fun>
|}];;

type _ j =
  | Has_A : [`A of int] j
  | Has_B : [`B of bool] j

let f : type a. a j -> a = function
  | Has_A -> `A 1
  | Has_B -> `B true
;;
[%%expect{|
type _ j = Has_A : [ `A of int ] j | Has_B : [ `B of bool ] j
val f : 'a j -> 'a = <fun>
|}];;

type (_,_) eq = Eq : ('a,'a) eq ;;

let f : type a b. (a,b) eq -> (<m : a; ..> as 'c) -> (<m : b; ..> as 'c) =
  fun Eq o -> o
;; (* fail *)
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq
Lines 3-4, characters 4-15:
3 | ....f : type a b. (a,b) eq -> (<m : a; ..> as 'c) -> (<m : b; ..> as 'c) =
4 |   fun Eq o -> o
Error: The universal type variable 'b cannot be generalized:
       it is already bound to another variable.
|}];;

let f : type a b. (a,b) eq -> <m : a; ..> -> <m : b; ..> =
  fun Eq o -> o
;; (* fail *)
[%%expect{|
Line 2, characters 14-15:
2 |   fun Eq o -> o
                  ^
Error: This expression has type < m : a; .. >
       but an expression was expected of type < m : b; .. >
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f (type a) (type b) (eq : (a,b) eq) (o : <m : a; ..>) : <m : b; ..> =
  match eq with Eq -> o ;; (* should fail *)
[%%expect{|
Line 2, characters 22-23:
2 |   match eq with Eq -> o ;; (* should fail *)
                          ^
Error: This expression has type < m : a; .. >
       but an expression was expected of type < m : b; .. >
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> <m : a> -> <m : b> =
  fun Eq o -> o
;; (* ok *)
[%%expect{|
val f : ('a, 'b) eq -> < m : 'a > -> < m : 'b > = <fun>
|}];;

let int_of_bool : (bool,int) eq = Obj.magic Eq;;

let x = object method m = true end;;
let y = (x, f int_of_bool x);;

let f : type a. (a, int) eq -> <m : a> -> bool =
  fun Eq o -> ignore (o : <m : int; ..>); o#m = 3
;; (* should be ok *)
[%%expect{|
val int_of_bool : (bool, int) eq = Eq
val x : < m : bool > = <obj>
val y : < m : bool > * < m : int > = (<obj>, <obj>)
val f : ('a, int) eq -> < m : 'a > -> bool = <fun>
|}];;

let f : type a b. (a,b) eq -> < m : a; .. > -> < m : b > =
  fun eq o ->
    ignore (o : < m : a >);
    let r : < m : b > = match eq with Eq -> o in (* fail with principal *)
    r;;
[%%expect{|
val f : ('a, 'b) eq -> < m : 'a > -> < m : 'b > = <fun>
|}, Principal{|
Line 4, characters 44-45:
4 |     let r : < m : b > = match eq with Eq -> o in (* fail with principal *)
                                                ^
Error: This expression has type < m : a >
       but an expression was expected of type < m : b >
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> < m : a; .. > -> < m : b > =
  fun eq o ->
    let r : < m : b > = match eq with Eq -> o in (* fail *)
    ignore (o : < m : a >);
    r;;
[%%expect{|
Line 3, characters 44-45:
3 |     let r : < m : b > = match eq with Eq -> o in (* fail *)
                                                ^
Error: This expression has type < m : a; .. >
       but an expression was expected of type < m : b >
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> [> `A of a] -> [> `A of b] =
  fun Eq o -> o ;; (* fail *)
[%%expect{|
Line 2, characters 14-15:
2 |   fun Eq o -> o ;; (* fail *)
                  ^
Error: This expression has type [> `A of a ]
       but an expression was expected of type [> `A of b ]
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f (type a b) (eq : (a,b) eq) (v : [> `A of a]) : [> `A of b] =
  match eq with Eq -> v ;; (* should fail *)
[%%expect{|
Line 2, characters 22-23:
2 |   match eq with Eq -> v ;; (* should fail *)
                          ^
Error: This expression has type [> `A of a ]
       but an expression was expected of type [> `A of b ]
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> [< `A of a | `B] -> [< `A of b | `B] =
  fun Eq o -> o ;; (* fail *)
[%%expect{|
Lines 1-2, characters 4-15:
1 | ....f : type a b. (a,b) eq -> [< `A of a | `B] -> [< `A of b | `B] =
2 |   fun Eq o -> o..............
Error: This definition has type
         'c. ('d, 'c) eq -> ([< `A of 'c & 'f & 'd | `B ] as 'e) -> 'e
       which is less general than
         'a 'b. ('a, 'b) eq -> ([< `A of 'b & 'h | `B ] as 'g) -> 'g
|}];;

let f : type a b. (a,b) eq -> [`A of a | `B] -> [`A of b | `B] =
  fun Eq o -> o ;; (* ok *)
[%%expect{|
val f : ('a, 'b) eq -> [ `A of 'a | `B ] -> [ `A of 'b | `B ] = <fun>
|}];;

let f : type a. (a, int) eq -> [`A of a] -> bool =
  fun Eq v -> match v with `A 1 -> true | _ -> false
;; (* ok *)
[%%expect{|
val f : ('a, int) eq -> [ `A of 'a ] -> bool = <fun>
|}];;

let f : type a b. (a,b) eq -> [> `A of a | `B] -> [`A of b | `B] =
  fun eq o ->
    ignore (o : [< `A of a | `B]);
    let r : [`A of b | `B] = match eq with Eq -> o in (* fail with principal *)
    r;;
[%%expect{|
val f : ('a, 'b) eq -> [ `A of 'a | `B ] -> [ `A of 'b | `B ] = <fun>
|}, Principal{|
Line 4, characters 49-50:
4 |     let r : [`A of b | `B] = match eq with Eq -> o in (* fail with principal *)
                                                     ^
Error: This expression has type [ `A of a | `B ]
       but an expression was expected of type [ `A of b | `B ]
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> [> `A of a | `B] -> [`A of b | `B] =
  fun eq o ->
    let r : [`A of b | `B] = match eq with Eq -> o in (* fail *)
    ignore (o : [< `A of a | `B]);
    r;;
[%%expect{|
Line 3, characters 49-50:
3 |     let r : [`A of b | `B] = match eq with Eq -> o in (* fail *)
                                                     ^
Error: This expression has type [> `A of a | `B ]
       but an expression was expected of type [ `A of b | `B ]
       Type a is not compatible with type b = a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}];;

(* Pattern matching *)

type 'a t =
    A of int | B of bool | C of float | D of 'a

type _ ty =
  | TE : 'a ty -> 'a array ty
  | TA : int ty
  | TB : bool ty
  | TC : float ty
  | TD : string -> bool ty

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
  | TD "bye", D false -> 13
  | TD "hello", D true -> 12
 (* | TB, D z -> if z then 1 else 2 *)
  | TC, D z -> truncate z
  | _, D _ -> 0
;;
[%%expect{|
type 'a t = A of int | B of bool | C of float | D of 'a
type _ ty =
    TE : 'a ty -> 'a array ty
  | TA : int ty
  | TB : bool ty
  | TC : float ty
  | TD : string -> bool ty
val f : 'a ty -> 'a t -> int = <fun>
|}];;

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
;; (* warn *)
[%%expect{|
Lines 2-8, characters 2-16:
2 | ..match x, y with
3 |   | _, A z -> z
4 |   | _, B z -> if z then 1 else 2
5 |   | _, C z -> truncate z
6 |   | TE TC, D [|1.0|] -> 14
7 |   | TA, D 0 -> -1
8 |   | TA, D z -> z
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(TE TC, D [| 0. |])
val f : 'a ty -> 'a t -> int = <fun>
|}];;

let f : type a. a ty -> a t -> int = fun x y ->
  match y, x with
  | A z, _ -> z
  | B z, _ -> if z then 1 else 2
  | C z, _ -> truncate z
  | D [|1.0|], TE TC -> 14
  | D 0, TA -> -1
  | D z, TA -> z
;; (* fail *)
[%%expect{|
Line 6, characters 6-13:
6 |   | D [|1.0|], TE TC -> 14
          ^^^^^^^
Error: This pattern matches values of type 'a array
       but a pattern was expected which matches values of type a
|}];;

type ('a,'b) pair = {right:'a; left:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* fail *)
[%%expect{|
type ('a, 'b) pair = { right : 'a; left : 'b; }
Line 8, characters 25-32:
8 |   | {left=TE TC; right=D [|1.0|]} -> 14
                             ^^^^^^^
Error: This pattern matches values of type 'a array
       but a pattern was expected which matches values of type a
|}];;

type ('a,'b) pair = {left:'a; right:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* ok *)
[%%expect{|
type ('a, 'b) pair = { left : 'a; right : 'b; }
Lines 4-10, characters 2-29:
 4 | ..match {left=x; right=y} with
 5 |   | {left=_; right=A z} -> z
 6 |   | {left=_; right=B z} -> if z then 1 else 2
 7 |   | {left=_; right=C z} -> truncate z
 8 |   | {left=TE TC; right=D [|1.0|]} -> 14
 9 |   | {left=TA; right=D 0} -> -1
10 |   | {left=TA; right=D z} -> z
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{left=TE TC; right=D [| 0. |]}
val f : 'a ty -> 'a t -> int = <fun>
|}];;

(* Injectivity *)

module M : sig type 'a t val eq : ('a t, 'b t) eq end =
  struct type 'a t = int let eq = Eq end
;;

let f : type a b. (a M.t, b M.t) eq -> (a, b) eq =
  function Eq -> Eq (* fail *)
;;
[%%expect{|
module M : sig type 'a t val eq : ('a t, 'b t) eq end
Line 6, characters 17-19:
6 |   function Eq -> Eq (* fail *)
                     ^^
Error: This expression has type (a, a) eq
       but an expression was expected of type (a, b) eq
       Type a is not compatible with type b
|}];;

let f : type a b. (a M.t * a, b M.t * b) eq -> (a, b) eq =
  function Eq -> Eq (* ok *)
;;
[%%expect{|
val f : ('a M.t * 'a, 'b M.t * 'b) eq -> ('a, 'b) eq = <fun>
|}];;

let f : type a b. (a * a M.t, b * b M.t) eq -> (a, b) eq =
  function Eq -> Eq (* ok *)
;;
[%%expect{|
val f : ('a * 'a M.t, 'b * 'b M.t) eq -> ('a, 'b) eq = <fun>
|}];;

(* Applications of polymorphic variants *)

type _ t =
  | V1 : [`A | `B] t
  | V2 : [`C | `D] t

let f : type a. a t -> a = function
  | V1 -> `A
  | V2 -> `C
;;

f V1;;
[%%expect{|
type _ t = V1 : [ `A | `B ] t | V2 : [ `C | `D ] t
val f : 'a t -> 'a = <fun>
- : [ `A | `B ] = `A
|}];;

(* PR#5425 and PR#5427 *)

type _ int_foo =
  | IF_constr : <foo:int; ..> int_foo

type _ int_bar =
  | IB_constr : <bar:int; ..> int_bar
;;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int>)
;;
[%%expect{|
type _ int_foo = IF_constr : < foo : int; .. > int_foo
type _ int_bar = IB_constr : < bar : int; .. > int_bar
Line 10, characters 3-4:
10 |   (x:<foo:int>)
        ^
Error: This expression has type t = < foo : int; .. >
       but an expression was expected of type < foo : int >
       Type $0 = < bar : int; .. > is not compatible with type <  >
       The second object type has no method bar
|}];;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int>)
;;
[%%expect{|
Line 3, characters 3-4:
3 |   (x:<foo:int;bar:int>)
       ^
Error: This expression has type t = < foo : int; .. >
       but an expression was expected of type < bar : int; foo : int >
       Type $0 = < bar : int; .. > is not compatible with type < bar : int >
       The first object type has an abstract row, it cannot be closed
|}];;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int;..>)
;;
[%%expect{|
Line 3, characters 2-26:
3 |   (x:<foo:int;bar:int;..>)
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type < bar : int; foo : int; .. >
       but an expression was expected of type 'a
       The type constructor $1 would escape its scope
|}, Principal{|
Line 3, characters 2-26:
3 |   (x:<foo:int;bar:int;..>)
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type < bar : int; foo : int; .. >
       but an expression was expected of type 'a
       This instance of $1 is ambiguous:
       it would escape the scope of its equation
|}];;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) : t =
  let IF_constr, IB_constr = e, e' in
  (x:<foo:int;bar:int;..>)
;;
[%%expect{|
val g : 't -> 't int_foo -> 't int_bar -> 't = <fun>
|}];;

let g (type t) (x:t) (e : t int_foo) (e' : t int_bar) =
  let IF_constr, IB_constr = e, e' in
  x, x#foo, x#bar
;;
[%%expect{|
val g : 't -> 't int_foo -> 't int_bar -> 't * int * int = <fun>
|}, Principal{|
Line 3, characters 5-10:
3 |   x, x#foo, x#bar
         ^^^^^
Error: This expression has type int but an expression was expected of type 'a
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

(* PR#5554 *)

type 'a ty = Int : int -> int ty;;

let f : type a. a ty -> a =
  fun x -> match x with Int y -> y;;

let g : type a. a ty -> a =
  let () = () in
  fun x -> match x with Int y -> y;;
[%%expect{|
type 'a ty = Int : int -> int ty
val f : 'a ty -> 'a = <fun>
val g : 'a ty -> 'a = <fun>
|}];;

(* Printing of anonymous variables *)

module M = struct type _ t = int end;;
module M = struct type _ t = T : int t end;;
module N = M;;
[%%expect{|
module M : sig type _ t = int end
module M : sig type _ t = T : int t end
module N = M
|}];;

(* Principality *)

(* adding a useless equation should not break inference *)
let f : type a b. (a,b) eq -> (a,int) eq -> a -> b -> _ = fun ab aint a b ->
  let Eq = ab in
  let x =
    let Eq = aint in
    if true then a else b
  in ignore x
;; (* ok *)
[%%expect{|
val f : ('a, 'b) eq -> ('a, int) eq -> 'a -> 'b -> unit = <fun>
|}];;

let f : type a b. (a,b) eq -> (b,int) eq -> a -> b -> _ = fun ab bint a b ->
  let Eq = ab in
  let x =
    let Eq = bint in
    if true then a else b
  in ignore x
;; (* ok *)
[%%expect{|
val f : ('a, 'b) eq -> ('b, int) eq -> 'a -> 'b -> unit = <fun>
|}];;

let f : type a b. (a,b) eq -> (a,int) eq -> a -> b -> _ = fun ab aint a b ->
  let Eq = aint in
  let x =
    let Eq = ab in
    if true then a else b
  in ignore x
;; (* ok *)
[%%expect{|
Line 5, characters 24-25:
5 |     if true then a else b
                            ^
Error: This expression has type b = int
       but an expression was expected of type a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let f : type a b. (a,b) eq -> (b,int) eq -> a -> b -> _ = fun ab bint a b ->
  let Eq = bint in
  let x =
    let Eq = ab in
    if true then a else b
  in ignore x
;; (* ok *)
[%%expect{|
Line 5, characters 24-25:
5 |     if true then a else b
                            ^
Error: This expression has type b = int
       but an expression was expected of type a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let f (type a b c) (b : bool) (w1 : (a,b) eq) (w2 : (a,int) eq) (x : a) (y : b) =
  let Eq = w1 in
  let Eq = w2 in
  if b then x else y
;;
[%%expect{|
Line 4, characters 19-20:
4 |   if b then x else y
                       ^
Error: This expression has type b = int
       but an expression was expected of type a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

let f (type a b c) (b : bool) (w1 : (a,b) eq) (w2 : (a,int) eq) (x : a) (y : b) =
  let Eq = w1 in
  let Eq = w2 in
  if b then y else x
[%%expect{|
Line 4, characters 19-20:
4 |   if b then y else x
                       ^
Error: This expression has type a = int
       but an expression was expected of type b = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}];;

module M = struct
  type t
end
type (_,_) eq = Refl: ('a,'a) eq
let f (x:M.t) (y: (M.t, int -> int) eq) =
  let Refl = y in
  if true then x else fun x -> x + 1
[%%expect{|
module M : sig type t end
type (_, _) eq = Refl : ('a, 'a) eq
Line 7, characters 22-36:
7 |   if true then x else fun x -> x + 1
                          ^^^^^^^^^^^^^^
Error: This expression has type 'a -> 'b
       but an expression was expected of type M.t = int -> int
       This instance of int -> int is ambiguous:
       it would escape the scope of its equation
|}]

(* Check got/expected when the order changes *)
module M = struct
  type t
end
type (_,_) eq = Refl: ('a,'a) eq
let f (x:M.t) (y: (M.t, int -> int) eq) =
  let Refl = y in
  if true then fun x -> x + 1 else x
[%%expect{|
module M : sig type t end
type (_, _) eq = Refl : ('a, 'a) eq
Line 7, characters 35-36:
7 |   if true then fun x -> x + 1 else x
                                       ^
Error: This expression has type M.t = int -> int
       but an expression was expected of type int -> int
       This instance of int -> int is ambiguous:
       it would escape the scope of its equation
|}]

module M = struct
  type t
end
type (_,_) eq = Refl: ('a,'a) eq
let f w (x:M.t) (y: (M.t, <m:int>) eq) =
  let Refl = y in
  let z = if true then x else w in
  z#m
[%%expect{|
module M : sig type t end
type (_, _) eq = Refl : ('a, 'a) eq
Line 8, characters 2-3:
8 |   z#m
      ^
Error: This expression has type M.t but an expression was expected of type
         < m : 'a; .. >
       This instance of < m : int > is ambiguous:
       it would escape the scope of its equation
|}]

(* Check got/expected when the order changes *)
module M = struct
  type t
end
type (_,_) eq = Refl: ('a,'a) eq
let f w (x:M.t) (y: (M.t, <m:int>) eq) =
  let Refl = y in
  let z = if true then w else x in
  z#m
[%%expect{|
module M : sig type t end
type (_, _) eq = Refl : ('a, 'a) eq
Line 8, characters 2-3:
8 |   z#m
      ^
Error: This expression has type M.t but an expression was expected of type
         < m : 'a; .. >
       This instance of < m : int > is ambiguous:
       it would escape the scope of its equation
|}]

type (_,_) eq = Refl: ('a,'a) eq
module M = struct
  type t = C : (<m:int; ..> as 'a) * ('a, <m:int; b:bool>) eq -> t
end
let f (C (x,y) : M.t) =
  let g w =
    let Refl = y in
    let z = if true then w else x in
    z#b
  in ()
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module M :
  sig
    type t =
        C : (< m : int; .. > as 'a) * ('a, < b : bool; m : int >) eq -> t
  end
Line 9, characters 4-5:
9 |     z#b
        ^
Error: This expression has type $C_'a = < b : bool >
       but an expression was expected of type < b : 'a; .. >
       This instance of < b : bool > is ambiguous:
       it would escape the scope of its equation
|}]

(* Check got/expected when the order changes *)
type (_,_) eq = Refl: ('a,'a) eq
module M = struct
  type t = C : (<m:int; ..> as 'a) * ('a, <m:int; b:bool>) eq -> t
end
let f (C (x,y) : M.t) =
  let g w =
    let Refl = y in
    let z = if true then x else w in
    z#b
  in ()
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module M :
  sig
    type t =
        C : (< m : int; .. > as 'a) * ('a, < b : bool; m : int >) eq -> t
  end
Line 9, characters 4-5:
9 |     z#b
        ^
Error: This expression has type $C_'a = < b : bool >
       but an expression was expected of type < b : 'a; .. >
       This instance of < b : bool > is ambiguous:
       it would escape the scope of its equation
|}]
