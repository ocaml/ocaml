(* TEST
   * expect
*)

let rec a = lazy b and b = 3;;
[%%expect{|
Line 1, characters 12-18:
1 | let rec a = lazy b and b = 3;;
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec e = lazy (fun _ -> f) and f = ();;
[%%expect{|
val e : ('a -> unit) lazy_t = lazy <fun>
val f : unit = ()
|}];;

let rec x = lazy (Lazy.force x + Lazy.force x)
  ;;
[%%expect{|
val x : int Lazy.t = <lazy>
|}];;
