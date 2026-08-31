(*TEST
  expect;
*)

(* Optional argument elision in argument of higher-order function *)
module type T = sig type t val x: t end
let higher (f: (module X:T) -> X.t) (module X:T) = f (module X)

let g ?(x=()) (module M:T) = M.x
[%%expect {|
module type T = sig type t val x : t end
val higher : ((module X : T) -> X.t) -> (module X : T) -> X.t = <fun>
val g : ?x:unit -> (module M : T) -> M.t = <fun>
|}]

let tfunctor_coerced = higher g
[%%expect {|
val tfunctor_coerced : (module X : T) -> X.t = <fun>
|}]

module A = struct type t = int let x = 0 end
module B = struct type t = string let x = "hello" end
let x = tfunctor_coerced (module A)
let y = tfunctor_coerced (module B)
[%%expect {|
module A : sig type t = int val x : int end
module B : sig type t = string val x : string end
val x : A.t = 0
val y : B.t = "hello"
|}]


(** Immediate hidden inside the module type *)
module type Imm = sig type t [@@immediate] val x: t end
let f ?x (module M:Imm) = M.x
let higher_imm (f: (module X:Imm) -> X.t) (module X:Imm) = f (module X)
let u = higher_imm f
let w = u (module struct type t = int let x = 0 end)
[%%expect {|
module type Imm = sig type t [@@immediate] val x : t end
val f : ?x:'a -> (module M : Imm) -> M.t = <fun>
val higher_imm : ((module X : Imm) -> X.t) -> (module X : Imm) -> X.t = <fun>
val u : (module X : Imm) -> X.t = <fun>
val w : int = 0
|}]


(** Effect order *)
let f ~x =
  Format.printf "First apply@.---------------------------------------@.";
  (fun ?y ->
     Format.printf "Opt apply@.";
     fun (module M:T) ->
       Format.printf "Last apply@.----------------@.";
       M.x
  )
let () = Format.printf "--(before u)------------@."
let u = higher (f ~x:0)
let () = Format.eprintf "--(after u)----------------------------@."
let w = u (module A)
let () = Format.eprintf "--(after w)-----@."
[%%expect {|
val f : x:'a -> ?y:'b -> (module M : T) -> M.t = <fun>
--(before u)------------
First apply
---------------------------------------
val u : (module X : T) -> X.t = <fun>
--(after u)----------------------------
Opt apply
Last apply
----------------
val w : A.t = 0
--(after w)-----
|}]

module type L = sig type t = x:int -> int end

let higher_lbl (f: (module M:L) -> M.t ) (module M:L) = f (module M)
let g ?x (module M:L): M.t = fun ~x -> x

(** No elision in presence of labeled arguments*)
let should_fail = higher_lbl g

[%%expect{|
module type L = sig type t = x:int -> int end
val higher_lbl : ((module M : L) -> M.t) -> (module M : L) -> M.t = <fun>
val g : ?x:'a -> (module M : L) -> M.t = <fun>
Line 7, characters 29-30:
7 | let should_fail = higher_lbl g
                                 ^
Error: The value "g" has type "?x:'a -> (module M : L) -> M.t"
       but an expression was expected of type "(module M : L) -> M.t"
       The first argument is labeled "?x",
       but an unlabeled argument was expected
|}]
