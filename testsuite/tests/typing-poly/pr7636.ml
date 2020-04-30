(* TEST
   * expect
*)

module M = struct
  type ('a, 'b) elt = 'a

  type 'a iter = { f : 'b.('a, 'b) elt -> unit }

  let promote (f : 'a -> unit) =
    let f : 'b.('a, 'b) elt -> unit = fun x -> f x in
    { f }
end
[%%expect{|
module M :
  sig
    type ('a, 'b) elt = 'a
    type 'a iter = { f : 'b. 'a -> unit; }
    val promote : ('a -> unit) -> 'a iter
  end
|}]

module M' : sig
  type ('a, 'b) elt
  type 'a iter = { f : 'b.('a, 'b) elt -> unit }
end = M
[%%expect{|
module M' :
  sig type ('a, 'b) elt type 'a iter = { f : 'b. ('a, 'b) elt -> unit; } end
|}]

type 'a t = int
let test : 'a. int -> 'a t = fun i -> i;;
[%%expect{|
type 'a t = int
val test : int -> int = <fun>
|}]
