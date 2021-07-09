(* TEST
   * expect
*)

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
[%%expect{|
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
|}]

let ok1 = function Dyn (type a) (w, x : a ty * a) -> ignore (x : a)
let ok2 = function Dyn (type a) (w, x : _ * a) -> ignore (x : a)
[%%expect{|
val ok1 : dyn -> unit = <fun>
val ok2 : dyn -> unit = <fun>
|}]

let ko1 = function Dyn (type a) (w, x) -> ()
[%%expect{|
Line 1, characters 32-38:
1 | let ko1 = function Dyn (type a) (w, x) -> ()
                                    ^^^^^^
Error: Existential types introduced in a constructor pattern
       must be bound by a type constraint on the argument.
|}]
let ko1 = function Dyn (type a) (w, x : _) -> ()
[%%expect{|
Line 1, characters 40-41:
1 | let ko1 = function Dyn (type a) (w, x : _) -> ()
                                            ^
Error: This type does not bind all existentials in the constructor:
         type a. 'a ty * 'a
|}]
let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
[%%expect{|
Line 1, characters 42-50:
1 | let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
                                              ^^^^^^^^
Error: This pattern matches values of type a ty * b
       but a pattern was expected which matches values of type a ty * a
       Type b is not compatible with type a
|}]

type u = C : 'a * ('a -> 'b list) -> u
let f = function C (type a b) (x, f : _ * (a -> b list)) -> ignore (x : a)
[%%expect{|
type u = C : 'a * ('a -> 'b list) -> u
val f : u -> unit = <fun>
|}]

let f = function C (type a) (x, f : a * (a -> a list)) -> ignore (x : a)
[%%expect{|
Line 1, characters 36-53:
1 | let f = function C (type a) (x, f : a * (a -> a list)) -> ignore (x : a)
                                        ^^^^^^^^^^^^^^^^^
Error: This type does not bind all existentials in the constructor:
         type a. a * (a -> a list)
|}]

(* with GADT unification *)
type _ expr =
  | Int : int -> int expr
  | Add : (int -> int -> int) expr
  | App : ('a -> 'b) expr * 'a expr -> 'b expr

let rec eval : type t. t expr -> t = function
    Int n -> n
  | Add -> (+)
  | App (type a) (f, x : _ * a expr) -> eval f (eval x : a)
[%%expect{|
type _ expr =
    Int : int -> int expr
  | Add : (int -> int -> int) expr
  | App : ('a -> 'b) expr * 'a expr -> 'b expr
val eval : 't expr -> 't = <fun>
|}]

let rec test : type a. a expr -> a = function
  | Int (type b) (n : a) -> n
  | Add -> (+)
  | App (type b) (f, x : (b -> a) expr * _) -> test f (test x : b)
[%%expect{|
Line 2, characters 22-23:
2 |   | Int (type b) (n : a) -> n
                          ^
Error: This type does not bind all existentials in the constructor: type b. a
|}]

(* Strange wildcard *)

[@@@warning "-28"]
let () =
  match None with
  | None (type a) (_ : a * int) -> ()
  | Some _ -> ()
[%%expect{|
Line 4, characters 4-31:
4 |   | None (type a) (_ : a * int) -> ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor None expects 0 argument(s),
       but is applied here to 1 argument(s)
|}]

let () =
  match None with
  | None _ -> ()
  | Some _ -> ()
[%%expect{|
|}]

(* Also allow annotations on multiary constructors *)
type ('a,'b) pair = Pair of 'a * 'b

let f = function Pair (x, y : int * _) -> x + y
[%%expect{|
type ('a, 'b) pair = Pair of 'a * 'b
val f : (int, int) pair -> int = <fun>
|}]
