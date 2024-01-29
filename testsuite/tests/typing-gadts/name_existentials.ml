(* TEST
 expect;
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
         "type a. 'a ty * 'a"
|}]
let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
[%%expect{|
Line 1, characters 42-50:
1 | let ko2 = function Dyn (type a b) (a, x : a ty * b) -> ignore (x : b)
                                              ^^^^^^^^
Error: The local name "b" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the name "a"
       that is already bound.
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
         "type a. a * (a -> a list)"
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
Error: The local name "b" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the type "'a"
       that is not a locally abstract type.
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
Error: The constructor "None" expects 0 argument(s),
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


(* #11891: allow naming more types *)
(* We stillonly allow to name freshly introduced existentials *)

type _ ty =
  | Int : int ty
  | Pair : 'b ty * 'c ty -> ('b * 'c) ty
let rec example : type a . a ty -> a = function
| Int -> 0
| Pair (x, y) -> (example x, example y)
let rec example : type a . a ty -> a = function
| Int -> 0
| Pair (type b c) (x, y : b ty * c ty) -> (example x, example y)
[%%expect{|
type _ ty = Int : int ty | Pair : 'b ty * 'c ty -> ('b * 'c) ty
val example : 'a ty -> 'a = <fun>
val example : 'a ty -> 'a = <fun>
|}]

let rec example : type a . a ty -> a = function
| Int -> 0
| Pair (type b c) (x, y : b ty * c ty) -> (example x, example (*error*)x)
[%%expect{|
Line 3, characters 54-72:
3 | | Pair (type b c) (x, y : b ty * c ty) -> (example x, example (*error*)x)
                                                          ^^^^^^^^^^^^^^^^^^
Error: This expression has type "b" = "$0" but an expression was expected of type
         "$1"
|}]

type _ th =
  | Thunk : 'a * ('a -> 'b) -> 'b th
let f1 (type a) : a th -> a = function
  | Thunk (type b) (x, f : b * (b -> _)) -> f x
let f2 (type a) : a th -> a = function
  | Thunk (type b c) (x, f : b * (b -> c)) -> f x
[%%expect{|
type _ th = Thunk : 'a * ('a -> 'b) -> 'b th
val f1 : 'a th -> 'a = <fun>
Line 6, characters 29-41:
6 |   | Thunk (type b c) (x, f : b * (b -> c)) -> f x
                                 ^^^^^^^^^^^^
Error: The local name "c" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the name "a"
       that was defined before.
|}]
(* Do not allow to deduce extra assumptions *)
let ko1 (type a) : a th -> a = function
  | Thunk (type b c) (x, f : b * (b -> c option)) -> f x
[%%expect{|
Line 2, characters 29-48:
2 |   | Thunk (type b c) (x, f : b * (b -> c option)) -> f x
                                 ^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "b * (b -> c option)"
       but a pattern was expected which matches values of type "b * (b -> a)"
       Type "c option" is not compatible with type "a"
|}]
(* Can only name fresh existentials *)
let ko2 = function
  | Thunk (type b c) (x, f : b * (b -> c)) -> f x
[%%expect{|
Line 2, characters 29-41:
2 |   | Thunk (type b c) (x, f : b * (b -> c)) -> f x
                                 ^^^^^^^^^^^^
Error: The local name "c" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the type "'a"
       that is not a locally abstract type.
|}]
let ko3 () =
  match [] with
  | [Thunk (type b c) (x, f : b * (b -> c))] -> f x
  | _ -> assert false
[%%expect{|
Line 3, characters 30-42:
3 |   | [Thunk (type b c) (x, f : b * (b -> c))] -> f x
                                  ^^^^^^^^^^^^
Error: The local name "c" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the type "'a"
       that is not a locally abstract type.
|}]

type _ tho =
  | Thunk_opt : 'b * ('b -> 'c option) -> 'c option tho
let f3 (type a) : a tho -> a = function
  | Thunk_opt (type b c) (x, f : b * (b -> c option)) -> f x
[%%expect{|
type _ tho = Thunk_opt : 'b * ('b -> 'c option) -> 'c option tho
val f3 : 'a tho -> 'a = <fun>
|}]


(* check locality *)
type 'a wrap = Wrap of 'a
type _ ty = Int : int ty | Pair : ('b ty * 'c ty) wrap -> ('b * 'c) ty
(* ok *)
let rec default : type a. a ty -> a = function
  | Int -> 0
  | Pair (type b c) (Wrap (b, c) : (b ty * c ty) wrap) ->
      (default b : b), (default c : c)
[%%expect{|
type 'a wrap = Wrap of 'a
type _ ty = Int : int ty | Pair : ('b ty * 'c ty) wrap -> ('b * 'c) ty
val default : 'a ty -> 'a = <fun>
|}]
(* ko *)
let rec default : type a. a ty -> a = function
  | Int -> 0
  | Pair (Wrap (type b c) (b, c : b ty * c ty)) ->
      (default b : b), (default c : c)
[%%expect{|
Line 3, characters 34-45:
3 |   | Pair (Wrap (type b c) (b, c : b ty * c ty)) ->
                                      ^^^^^^^^^^^
Error: The local name "b" can only be given to an existential variable
       introduced by this GADT constructor.
       The type annotation tries to bind it to the name "$0"
       that was defined before.
|}]
