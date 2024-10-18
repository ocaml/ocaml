(* TEST
 expect;
*)
type t =
  { x : int
  ; y : int
  }

let sum ({ x; y } as t) = x + y

[%%expect{|
type t = { x : int; y : int; }
Line 6, characters 21-22:
6 | let sum ({ x; y } as t) = x + y
                         ^
Warning 26 [unused-var]: unused variable "t".

val sum : t -> int = <fun>
|}]
