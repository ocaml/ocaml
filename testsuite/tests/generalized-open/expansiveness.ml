(* TEST
   * expect
*)

module Fn = struct
  let id x = x
end
;;
[%%expect{|
module Fn : sig val id : 'a -> 'a end
|}]

let f = fun x -> Fn.id x
;;
[%%expect{|
val f : 'a -> 'a = <fun>
|}]

let g = Fn.(fun x -> id x)
let h = let open Fn in fun x -> id x
;;
[%%expect{|
val g : '_weak1 -> '_weak1 = <fun>
val h : '_weak2 -> '_weak2 = <fun>
|}]

let i =
  let open struct
    let id x = x
  end in
  fun x -> id x

let iM =
  let module M = struct
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val i : '_weak3 -> '_weak3 = <fun>
val iM : 'a -> 'a = <fun>
|}]

let j =
  let open struct
    exception E
    let id x = x
  end in
  fun x -> id x

let jM =
  let module M = struct
    exception E
    let id x = x
  end in
  fun x -> M.id x
;;
[%%expect{|
val j : '_weak4 -> '_weak4 = <fun>
val jM : '_weak5 -> '_weak5 = <fun>
|}]

module Square(X : sig val x : int end) = struct
  let result = X.x * X.x
end
;;
[%%expect{|
module Square : functor (X : sig val x : int end) -> sig val result : int end
|}]

let k =
  let open Square(struct let x = 3 end) in
  fun x -> x

let kM =
  let module M = Square(struct let x = 3 end) in
  fun x -> x
;;
[%%expect{|
val k : '_weak6 -> '_weak6 = <fun>
val kM : '_weak7 -> '_weak7 = <fun>
|}]
