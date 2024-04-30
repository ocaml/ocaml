(* TEST
   expect;
*)

module type T = sig
  type t
end

module type T2 = sig
  type t
end

module type Add = sig
  type t
  val add : t -> t -> t
end

[%%expect{|
module type T = sig type t end
module type T2 = sig type t end
module type Add = sig type t val add : t -> t -> t end
|}]

type t1 =
  | A1 of (int -> int)
  | B1 of ((module M : T) -> M.t -> M.t)

let f = function
  | A1 f -> f 1
  | B1 f -> f (module Int) 2

[%%expect{|
type t1 = A1 of (int -> int) | B1 of ((module M : T) -> M.t -> M.t)
val f : t1 -> Int.t = <fun>
|}]

type _ t2 =
  | A : (int -> int) t2
  | B : ((module M : T) -> M.t -> M.t) t2
  | C : ((module M : T2) -> M.t -> M.t) t2
  | D : ((module M : Add) -> M.t -> M.t) t2

[%%expect{|
type _ t2 =
    A : (int -> int) t2
  | B : ((module M : T) -> M.t -> M.t) t2
  | C : ((module M : T2) -> M.t -> M.t) t2
  | D : ((module M : Add) -> M.t -> M.t) t2
|}]

(* matching specification *)

let f (type a) (x : a) (el : a t2) =
  match el, x with
  | A, f -> f 1
  | B, f -> f (module Int) 2
  | C, f -> f (module Int) 3
  | D, f -> f (module Int) 4

[%%expect{|
val f : 'a -> 'a t2 -> int = <fun>
|}, Principal{|
Line 3, characters 12-15:
3 |   | A, f -> f 1
                ^^^
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]


(* escape errors *)

let f (type a) (x : a) (el : ((module N : T) -> a) t2) =
  match el, x with
  | B, f -> f
  | C, f -> f

[%%expect{|
Line 3, characters 4-5:
3 |   | B, f -> f
        ^
Error: This pattern matches values of type "((module M : T) -> M.t -> M.t) t2"
       but a pattern was expected which matches values of type
         "((module N : T) -> a) t2"
       The module "M" would escape its scope
|}]
