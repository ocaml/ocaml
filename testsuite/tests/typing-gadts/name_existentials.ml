(* TEST
   * expect
*)

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
[%%expect{|
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
|}]

let ok1 = function Dyn (type a) ((w : a ty), x) -> ignore (x : a)
let ok2 = function Dyn (type a) (w, (x : a)) -> ignore (x : a)
[%%expect{|
val ok1 : dyn -> unit = <fun>
val ok2 : dyn -> unit = <fun>
|}]

let ko1 = function Dyn (type a) (w, x) -> ()
[%%expect{|
Line 1, characters 19-38:
1 | let ko1 = function Dyn (type a) (w, x) -> ()
                       ^^^^^^^^^^^^^^^^^^^
Error: This type does not bind all existentials in the constructor:
         type a. 'a ty * 'a
|}]
let ko2 = function Dyn (type a b) ((a : a ty), (x : b)) -> ignore (x : b)
[%%expect{|
Line 1, characters 52-53:
1 | let ko2 = function Dyn (type a b) ((a : a ty), (x : b)) -> ignore (x : b)
                                                        ^
Error: This pattern matches values of type b
       but a pattern was expected which matches values of type a
|}]

type u = C : 'a * ('a -> 'b list) -> u
let f = function C (type a b) (x, (f : a -> b list)) -> ignore (x : a)
[%%expect{|
type u = C : 'a * ('a -> 'b list) -> u
val f : u -> unit = <fun>
|}]

let f = function C (type a) ((x : a), (f : a -> a list)) -> ignore (x : a)
[%%expect{|
Line 1, characters 17-56:
1 | let f = function C (type a) ((x : a), (f : a -> a list)) -> ignore (x : a)
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
  | App (type a) (f, (x : a expr)) -> eval f (eval x : a)
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
  | App (type b) ((f : (b -> a) expr), x) -> test f (test x : b)
[%%expect{|
val test : 'a expr -> 'a = <fun>
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
