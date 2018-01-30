(* TEST
   * expect
*)

type ('a, 'b) elt = 'a

type 'a iter = { f : 'b.('a, 'b) elt -> unit }

let promote (f : 'a -> unit) =
 let f : 'b.('a, 'b) elt -> unit = fun x -> f x in 
 { f }
[%%expect{|
type ('a, 'b) elt = 'a
type 'a iter = { f : 'b. ('a, 'b) elt -> unit; }
val promote : (('a, 'b) elt -> unit) -> 'a iter = <fun>
|}]

type 'a t = int
let test : 'a. int -> 'a t = fun i -> i;;
[%%expect{|
type 'a t = int
val test : 'a t -> 'a0 t = <fun>
|}, Principal{|
type 'a t = int
val test : int -> 'a t = <fun>
|}]
