(* TEST
   expect;
*)

type t1 = A
type t2 = A

[%%expect{|
type t1 = A
type t2 = A
|}]

let f x = match (x :> t1) with
  | A -> 1

[%%expect{|
val f : t1 -> int = <fun>
|}]
