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

let _ =
  let type t = A of int | B in
  let _ = [A 42; B] in
  let type t = .. in
  let type t += A of string in
  let _ = A "hello" in
  let class c = object method f = 42 end in
  let class type ct = object method f : int end in
  let class d : ct = object (self) inherit c initializer print_int (self # f) end in
  let external f : 'a -> 'a = "%identity" in
  let [@@@warning "-unused-var"] in
  let v = (42, 12) in
  assert (f v == v);
  "OK"
[%%expect{|
- : string = "OK"
|}]
