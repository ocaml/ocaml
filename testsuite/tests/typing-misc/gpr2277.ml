(* TEST
   * expect
*)

let f (type t) (x : t) = x

[%%expect {|
val f : 't -> 't = <fun>
|}]

let g (type t') (x : t') = x

let g' (x : ' t') = x

[%%expect {|
val g : ' t' -> ' t' = <fun>
val g' : ' t' -> ' t' = <fun>
|}]

let h (type a'bc) (x : a'bc) = x

let h' (x : ' a'bc) = x

[%%expect {|
val h : ' a'bc -> ' a'bc = <fun>
val h' : ' a'bc -> ' a'bc = <fun>
|}]

let i (type fst snd) (x : fst) (y : snd) = (x, y)

[%%expect {|
val i : 'fst -> 'snd -> 'fst * 'snd = <fun>
|}]

let j (type fst snd fst' snd') (x : fst) (y : snd) (a : fst') (b : snd') =
  ((x, y), (a, b))

[%%expect {|
val j : 'fst -> 'snd -> 'fst' -> 'snd' -> ('fst * 'snd) * ('fst' * 'snd') =
  <fun>
|}]

(* Variable names starting with _ are reserved for the compiler. *)
let k (type _weak1) (x : _weak1) = x

[%%expect {|
val k : 'a -> 'a = <fun>
|}]

let l (type _') (x : _') = x

[%%expect {|
val l : 'a -> 'a = <fun>
|}]
