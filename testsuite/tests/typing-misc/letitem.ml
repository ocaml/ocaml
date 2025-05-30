(* TEST
 expect;
*)

let _ =
  let type t = A in
  A
[%%expect{|
Line 3, characters 2-3:
3 |   A
      ^
Error: The constructor "A" has type "t" but an expression was expected of type "'a"
       The type constructor "t" would escape its scope
|}];;

let _ =
  let type t = .. in
  let type t += A in
  A
  [%%expect{|
Line 4, characters 2-3:
4 |   A
      ^
Error: The constructor "A" has type "t" but an expression was expected of type "'a"
       The type constructor "t" would escape its scope
|}];;

type u = ..

let _ =
  let type u += A in
  A
  [%%expect{|
type u = ..
- : u = <extension>
|}];;

let _ =
  let class c = object method f = 12 end in
  new c
  [%%expect{|
- : < f : int > = <obj>
|}];;

let _ =
  let external f : 'a -> 'a = "%identity" in
  f
[%%expect{|
- : 'a -> 'a = <fun>
|}];;
