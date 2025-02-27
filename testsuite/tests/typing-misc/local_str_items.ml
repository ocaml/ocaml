(* TEST
 expect;
*)

(* Local types *)

let f x1 y1 x2 y2 =
  let type pt = {x: int; y: int} in
  let dist p1 p2 = (p2.x - p1.x) + (p2.y - p1.y) in
  dist {x = x1; y = y1} {x = x2; y = y2}
;;
[%%expect{|
val f : int -> int -> int -> int -> int = <fun>
|}];;

let f x y =
  let type pt = {x: int; y: int} in
  {x; y}
;;
[%%expect{|
Line 3, characters 2-8:
3 |   {x; y}
      ^^^^^^
Error: This expression has type "pt" but an expression was expected of type "'a"
       The type constructor "pt" would escape its scope
|}];;

(* Local type extensions *)

type t = ..

let f () =
  let type t += A of int in
  A 12
;;
[%%expect{|
type t = ..
val f : unit -> t = <fun>
|}];;

(* Local module types *)

let f () =
  let module type T = sig type t end in
  let module _ : T = struct type s end in
  ()
;;
[%%expect{|
Line 3, characters 21-38:
3 |   let module _ : T = struct type s end in
                         ^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig type s end is not included in T
       The type "t" is required but not provided
|}];;

(* Local opens *)

let f () =
  let open Set.Make(String) in
  of_list ["hola"]
;;
[%%expect{|
val f : unit -> Set.Make(String).t = <fun>
|}];;

let f () =
  let module type T = sig type t val x : t end in
  let module M : T = struct type t = int let x = 0 end in
  M.x
;;
[%%expect{|
Line 4, characters 2-5:
4 |   M.x
      ^^^
Error: The value "M.x" has type "M.t" but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

(* Local classes *)

let _ =
  let class c = object method f = 42 end in
  (new c)#f
;;
[%%expect{|
- : int = 42
|}];;

let _ =
  let class c = object method f = 42 end in
  new c
;;
[%%expect{|
- : < f : int > = <obj>
|}];;

let _ =
  let o =
    let class c = object method f = 42 end in
    object(self) inherit c method g = self#f + 1 end
  in
  o#g
;;
[%%expect{|
- : int = 43
|}];;

(* Local class types *)

let _ =
  let class type c = object method f : int end in
  let c : c = object method g = 42 end in
  c
;;
[%%expect{|
Line 3, characters 14-38:
3 |   let c : c = object method g = 42 end in
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "< g : int >"
       but an expression was expected of type "c"
       The second object type has no method "g"
|}];;

(* Local attributes *)

let _ =
  let x = 42 in
  ()
;;
[%%expect{|
Line 2, characters 6-7:
2 |   let x = 42 in
          ^
Warning 26 [unused-var]: unused variable "x".

- : unit = ()
|}];;

let _ =
  let [@@@warning "-26"] in
  let x = 42 in
  ()
;;
[%%expect{|
- : unit = ()
|}];;

(* Local exceptions *)

let _ =
  let exception E of int in
  let rec loop i =
    if i = 10 then raise (E i)
    else loop (i+1)
  in
  match loop 0 with
  | exception E 10 -> ()
  | () -> assert false
;;
[%%expect{|
- : unit = ()
|}];;

(* Local modules *)

let _ =
  let module S = Set.Make(String) in
  S.elements (S.of_list ["hola"])
;;
[%%expect{|
- : String.t list = ["hola"]
|}];;

(* Local primitives *)

let _ =
  let external f : 'a -> 'a = "%identity" in
  (f 42, f "hello")
;;
[%%expect{|
- : int * string = (42, "hello")
|}];;

let _ =
  let external f : 'a -> 'a = "%identity" in
  f
;;
[%%expect{|
- : 'a -> 'a = <fun>
|}];;
