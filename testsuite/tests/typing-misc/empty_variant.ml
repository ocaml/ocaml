(* TEST
   * expect
*)

(* empty variant *)
type t = |;;
[%%expect{|
type t = |
|}];;

let f (x:t) = match x with _ -> .
[%%expect{|
val f : t -> 'a = <fun>
|}];;

type m = A of t | B of int * t | C of {g:t}
[%%expect{|
type m = A of t | B of int * t | C of { g : t; }
|}]

let g (x:m) =
  match x with
  | A _ | B _ | C _ -> .
[%%expect{|
val g : m -> 'a = <fun>
|}]

let f : t option -> int = function None -> 3 
[%%expect{|
val f : t option -> int = <fun>
|}]
