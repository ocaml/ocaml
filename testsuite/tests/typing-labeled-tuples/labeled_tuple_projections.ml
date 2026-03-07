(* TEST
   expect;
*)

(* Basic example *)
let point = ~x:1, ~y:2
let x_coord = point.~x
let y_coord = point.~y
[%%expect{|
val point : x:int * y:int = (~x:1, ~y:2)
val x_coord : int = 1
val y_coord : int = 2
|}]

(* (~x:v1, ...).~x = v1 *)
let _ = (~x:"hello", ~y:"world").~y
[%%expect{|
- : string = "world"
|}]

(* Mixed tuples *)
let mixed = ~a:1, 2, ~c:3
let a_field = mixed.~a
let c_field = mixed.~c
[%%expect{|
val mixed : a:int * int * c:int = (~a:1, 2, ~c:3)
val a_field : int = 1
val c_field : int = 3
|}]

let bad_b_field = mixed.~b
[%%expect{|
Line 1, characters 25-26:
1 | let bad_b_field = mixed.~b
                             ^
Error: No field "b" for the tuple "a:int * int * c:int".
|}]

type xy = x:int * y:int

let distance (p1 : xy) (p2 : xy) =
  let dx = p1.~x - p2.~x in
  let dy = p1.~y - p2.~y in
  sqrt (float_of_int (dx * dx + dy * dy))
[%%expect{|
type xy = x:int * y:int
val distance : xy -> xy -> float = <fun>
|}]

let _ = distance (~x:0, ~y:0) (~x:3, ~y:4)
[%%expect{|
- : float = 5.
|}]

(* Nested projections *)
let nested = ~outer:(~inner:42, "text"), ~value:"hello"
let _ = nested.~outer.~inner
[%%expect{|
val nested : outer:(inner:int * string) * value:string =
  (~outer:(~inner:42, "text"), ~value:"hello")
- : int = 42
|}]

(* Polymorphism *)
let swap (pair : fst:'a * snd:'b) = ~fst:pair.~snd, ~snd:pair.~fst
[%%expect{|
val swap : (fst:'a * snd:'b) -> fst:'b * snd:'a = <fun>
|}]

let _ = swap (~fst:1, ~snd:"hello")
let _ = swap (~fst:true, ~snd:3.14)
[%%expect{|
- : fst:string * snd:int = (~fst:"hello", ~snd:1)
- : fst:float * snd:bool = (~fst:3.14, ~snd:true)
|}]

(* Projections on function parameters *)
let points = [~x:1, ~y:2; ~x:3, ~y:4]
let x_coords = List.map (fun (p : xy) -> p.~x) points
[%%expect{|
val points : (x:int * y:int) list = [(~x:1, ~y:2); (~x:3, ~y:4)]
val x_coords : int list = [1; 3]
|}]

(* Error: Projection from non-tuple *)
let not_a_tuple = 42
let _ = not_a_tuple.~x
[%%expect{|
val not_a_tuple : int = 42
Line 2, characters 8-19:
2 | let _ = not_a_tuple.~x
            ^^^^^^^^^^^
Error: This expression has type "int" which is not a tuple.
|}]

(* Error: Label not found *)
let point2d = ~x:1, ~y:2
let _ = point2d.~z
[%%expect{|
val point2d : x:int * y:int = (~x:1, ~y:2)
Line 2, characters 17-18:
2 | let _ = point2d.~z
                     ^
Error: No field "z" for the tuple "x:int * y:int".
|}]

(* Error: Unlabeled tuple *)
let unlabeled = 1, 2
let _ = unlabeled.~x
[%%expect{|
val unlabeled : int * int = (1, 2)
Line 2, characters 19-20:
2 | let _ = unlabeled.~x
                       ^
Error: No field "x" for the tuple "int * int".
|}]

(* Type propagation :) *)
let id x = x
let _ = (id (~x:1, ~y:2)).~x
[%%expect{|
val id : 'a -> 'a = <fun>
- : int = 1
|}]

let _ = (id (~x:1, ~y:2) : xy).~x
[%%expect{|
- : int = 1
|}]
