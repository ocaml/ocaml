(* TEST
   * expect
*)

type t = T0 | T1 of t | Tf of (unit -> t)
[%%expect{|
type t = T0 | T1 of t | Tf of (unit -> t)
|}]
;;

let rec a = T0
and b =
  let rec f () = T1 a
  and x = Tf f
  in x
[%%expect{|
val a : t = T0
val b : t = Tf <fun>
|}]
;;

let rec a = T0
and b =
  let rec f () = T1 a
  and x = Tf f
  and y = ref "foo"
  in !y, x
[%%expect{|
val a : t = T0
val b : string * t = ("foo", Tf <fun>)
|}]
;;
