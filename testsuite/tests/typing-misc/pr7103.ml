(* TEST
 expect;
*)

type 'a t
type a

let f : < .. > t -> unit = fun _ -> ();;

let g : [< `b] t -> unit = fun _ -> ();;

let h : [> `b] t -> unit = fun _ -> ();;
[%%expect{|
type 'a t
type a
val f : < .. > t -> unit = <fun>
val g : [< `b ] t -> unit = <fun>
val h : [> `b ] t -> unit = <fun>
|}];;

let _ = fun (x : a t) -> f x;;
[%%expect{|
Line 1, characters 27-28:
1 | let _ = fun (x : a t) -> f x;;
                               ^
Error: The value "x" has type "a t" but an expression was expected of type
         "< .. > t"
       Type "a" is not compatible with type "< .. >"
|}];;

let _ = fun (x : a t) -> g x;;
[%%expect{|
Line 1, characters 27-28:
1 | let _ = fun (x : a t) -> g x;;
                               ^
Error: The value "x" has type "a t" but an expression was expected of type
         "[< `b ] t"
       Type "a" is not compatible with type "[< `b ]"
|}];;

let _ = fun (x : a t) -> h x;;
[%%expect{|
Line 1, characters 27-28:
1 | let _ = fun (x : a t) -> h x;;
                               ^
Error: The value "x" has type "a t" but an expression was expected of type
         "[> `b ] t"
       Type "a" is not compatible with type "[> `b ]"
|}];;
