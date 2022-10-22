(* TEST
   * expect
*)

(* from @dyzsr *)
type 'a t = T : ('a -> 'b) * ('b -> 'a) -> 'a t;;
[%%expect{|
type 'a t = T : ('a -> 'b) * ('b -> 'a) -> 'a t
|}]

let t = T ((fun x -> x), (fun x -> x));;
[%%expect{|
val t : 'a t = T (<fun>, <fun>)
|}]

let t1 = let T (g, h) = t in h (g 1);;
[%%expect{|
val t1 : int = 1
|}]

let f x = let T (g, h) = t in h (g x);;
[%%expect{|
val f : 'a -> 'a = <fun>
|}]

(* reformulation by @gasche *)

(* an isomorphism between 'a and 'b *)
type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)

(* exists 'b. ('a, 'b) iso *)
type 'a some_iso = Iso : ('a, 'b) iso -> 'a some_iso
[%%expect{|
type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)
type 'a some_iso = Iso : ('a, 'b) iso -> 'a some_iso
|}]

(* forall 'a. exists 'b. ('a, 'b) iso *)
let t : 'a . 'a some_iso =
  Iso ((fun x -> x), (fun x -> x))
[%%expect{|
val t : 'a some_iso = Iso (<fun>, <fun>)
|}]

let unsound_cast : 'a 'b. 'a -> 'b = fun x ->
  match t with Iso (g, h) -> h (g x)
[%%expect{|
Lines 1-2, characters 37-36:
1 | .....................................fun x ->
2 |   match t with Iso (g, h) -> h (g x)
Error: This definition has type 'c. 'c -> 'c which is less general than
         'a 'b. 'a -> 'b
|}]
