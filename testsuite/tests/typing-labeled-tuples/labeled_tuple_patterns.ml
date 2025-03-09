(* TEST
   expect;
*)

(* Test match statements with exception patterns *)
exception Odd

let x_must_be_even (~x, y) =
   if x mod 2 = 1 then
      raise Odd
   else
      (~x, y)

let foo xy k_good k_bad =
   match x_must_be_even xy with
   | (~x, y) -> k_good ()
   | exception Odd -> k_bad ()
[%%expect{|
exception Odd
val x_must_be_even : (x:int * 'a) -> x:int * 'a = <fun>
val foo : (x:int * 'a) -> (unit -> 'b) -> (unit -> 'b) -> 'b = <fun>
|}]

(* Test correctness *)
let _ = foo (~x:2, 5) (fun () -> true) (fun () -> false)
let _ = foo (~x:3, 5) (fun () -> false) (fun () -> true)
[%%expect{|
- : bool = true
- : bool = true
|}]

(* Test that the actions occur outside of the exception handler *)
let _ =
   try
      foo (~x:2, 5) (fun () -> raise Odd) (fun () -> false)
   with Odd -> true
let _ =
   try
      foo (~x:3, 5) (fun () -> false) (fun () -> raise Odd)
   with Odd -> true
[%%expect{|
- : bool = true
- : bool = true
|}]

(* Labeled tuple pattern *)
let (~x:x0, ~y:y0, _) = ~x: 1, ~y: 2, "ignore me"
[%%expect{|
val x0 : int = 1
val y0 : int = 2
|}]

(* Pattern with punning and type annotation *)
let (~(x:int), ~y, _) = ~x: 1, ~y: 2, "ignore me"
[%%expect{|
val x : int = 1
val y : int = 2
|}]

(* Patterns in functions *)
let f = fun (~foo, ~bar:bar) -> foo * 10 + bar
let bar = 5
let _ = f (~foo:1, ~bar)
[%%expect{|
val f : (foo:int * bar:int) -> int = <fun>
val bar : int = 5
- : int = 15
|}]

(* Correct annotation *)
let f : (foo:int * bar:int) -> int =
   fun (~foo, ~bar:bar) -> foo * 10 + bar
[%%expect{|
val f : (foo:int * bar:int) -> int = <fun>
|}]

let f = fun (~foo, ~bar:bar) : (foo:int * bar:int) -> foo * 10 + bar
[%%expect{|
Line 1, characters 54-68:
1 | let f = fun (~foo, ~bar:bar) : (foo:int * bar:int) -> foo * 10 + bar
                                                          ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "foo:int * bar:int"
|}]

(* Missing label *)
let f : (int * bar:int) -> int = fun (~foo, ~bar:bar) -> foo * 10 + bar
[%%expect{|
Line 1, characters 37-53:
1 | let f : (int * bar:int) -> int = fun (~foo, ~bar:bar) -> foo * 10 + bar
                                         ^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type "int * bar:int",
       but it is missing an unlabeled component.
|}]

let f = fun (~foo, ~bar:bar) : (foo:int * int) -> foo * 10 + bar
[%%expect{|
Line 1, characters 50-64:
1 | let f = fun (~foo, ~bar:bar) : (foo:int * int) -> foo * 10 + bar
                                                      ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "foo:int * int"
|}]

(* Wrong label *)
let f : (foo:int * foo':int) -> int =
   fun (~foo, ~bar:bar) -> foo * 10 + bar
[%%expect{|
Line 2, characters 7-23:
2 |    fun (~foo, ~bar:bar) -> foo * 10 + bar
           ^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type "foo:int * foo':int",
       but it is missing a component with label "foo'".
       Hint: use .. to ignore some components.
|}]

(* Wrong type *)
let f : (foo:float * foo':int) -> int =
   fun (~foo, ~bar:bar) -> foo * 10 + bar
[%%expect{|
Line 2, characters 7-23:
2 |    fun (~foo, ~bar:bar) -> foo * 10 + bar
           ^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "foo:float * foo':int", but it is missing a component with label "foo'".
       Hint: use .. to ignore some components.
|}]

(* Annotated pattern *)
let f (~x,y : (x:int * int)) : int = x + y
[%%expect{|
val f : (x:int * int) -> int = <fun>
|}]

(* Misannotated pattern *)
let f (~x,y : (int * int)) : int = x + y
[%%expect{|
Line 1, characters 7-11:
1 | let f (~x,y : (int * int)) : int = x + y
           ^^^^
Error: This pattern was expected to match values of type "int * int",
       but it is missing an unlabeled component.
|}]

let f (~x,y : (int * x:int)) : int = x + y
[%%expect{|
val f : int * x:int -> int = <fun>
|}]

(* Annotation within pattern *)
let f (~(x:int),y : (x:int * int)) : int = x + y
[%%expect{|
val f : (x:int * int) -> int = <fun>
|}]

let f (~(x:int),y) = x + y
[%%expect{|
val f : (x:int * int) -> int = <fun>
|}]

let f (~x:(x0:int),y) = x0 + y
[%%expect{|
val f : (x:int * int) -> int = <fun>
|}]

(* Misannotation within pattern *)
let f (~(x:float),y) = x + y
[%%expect{|
Line 1, characters 23-24:
1 | let f (~(x:float),y) = x + y
                           ^
Error: The value "x" has type "float" but an expression was expected of type "int"
|}]
(* Reordering in functions *)
type xy = (x:int * y:int)
type yx = (y:int * x:int)
let xy_id (pt : xy) = pt
let yx_id (pt : yx) = pt
[%%expect{|
type xy = x:int * y:int
type yx = y:int * x:int
val xy_id : xy -> xy = <fun>
val yx_id : yx -> yx = <fun>
|}]

let xy_id (~y, ~x) : xy = ~x, ~y
[%%expect{|
val xy_id : (y:int * x:int) -> xy = <fun>
|}]


let swap (~x, ~y) = ~y, ~x
[%%expect{|
val swap : (x:'a * y:'b) -> y:'b * x:'a = <fun>
|}]

let swap (~y, ~x : xy) = ~y, ~x
[%%expect{|
val swap : xy -> y:int * x:int = <fun>
|}]

let swap (~x, ~y) = (~x, ~y : yx)
[%%expect{|
Line 1, characters 21-27:
1 | let swap (~x, ~y) = (~x, ~y : yx)
                         ^^^^^^
Error: This expression has type "x:'a * y:'b"
       but an expression was expected of type "yx" = "y:int * x:int"
|}]

let swap (pt : xy) : yx = pt
[%%expect{|
Line 1, characters 26-28:
1 | let swap (pt : xy) : yx = pt
                              ^^
Error: The value "pt" has type "xy" = "x:int * y:int"
       but an expression was expected of type "yx" = "y:int * x:int"
|}]

let swap : xy -> yx = Fun.id
[%%expect{|
Line 1, characters 22-28:
1 | let swap : xy -> yx = Fun.id
                          ^^^^^^
Error: The value "Fun.id" has type "xy -> xy"
       but an expression was expected of type "xy -> yx"
       Type "xy" = "x:int * y:int" is not compatible with type "yx" = "y:int * x:int"
|}]

let swap : xy -> yx = xy_id
[%%expect{|
Line 1, characters 22-27:
1 | let swap : xy -> yx = xy_id
                          ^^^^^
Error: The value "xy_id" has type "(y:int * x:int) -> xy"
       but an expression was expected of type "xy -> yx"
       Type "y:int * x:int" is not compatible with type "xy" = "x:int * y:int"
|}]

let swap : xy -> yx = yx_id
[%%expect{|
Line 1, characters 22-27:
1 | let swap : xy -> yx = yx_id
                          ^^^^^
Error: The value "yx_id" has type "yx -> yx"
       but an expression was expected of type "xy -> yx"
       Type "yx" = "y:int * x:int" is not compatible with type "xy" = "x:int * y:int"
|}]

(* Reordering and partial matches *)
let lt = ~x:1, 2, ~y:3, ~z:4, 5, 6

(* Full match, in order *)
let matches =
  let ~x, k1, ~y, ~z, k2, k3 = lt in
  x, k1, y, z, k2, k3
[%%expect{|
val lt : x:int * int * y:int * z:int * int * int =
  (~x:1, 2, ~y:3, ~z:4, 5, 6)
val matches : int * int * int * int * int * int = (1, 2, 3, 4, 5, 6)
|}]

(* Full match, over-bound *)
let matches =
  let ~x, k1, ~y, ~z, x, k3 = lt in
  ()
[%%expect{|
Line 2, characters 22-23:
2 |   let ~x, k1, ~y, ~z, x, k3 = lt in
                          ^
Error: Variable "x" is bound several times in this matching
|}]

let matches =
  let ~x, k1, ~y, ~z:x, k2, k3 = lt in
  ()
[%%expect{|
Line 2, characters 21-22:
2 |   let ~x, k1, ~y, ~z:x, k2, k3 = lt in
                         ^
Error: Variable "x" is bound several times in this matching
|}]

(* Full match, missing label *)
let matches =
  let ~x, k1, ~y, k2, k3 = lt in
  ()
[%%expect{|
Line 2, characters 6-24:
2 |   let ~x, k1, ~y, k2, k3 = lt in
          ^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * int * y:int * z:int * int * int",
       but it is missing a component with label "z".
       Hint: use .. to ignore some components.
|}]

(* Full match, wrong label *)
let matches =
  let ~x, k1, ~y, ~w, k2, k3 = lt in
  ()
[%%expect{|
Line 2, characters 6-28:
2 |   let ~x, k1, ~y, ~w, k2, k3 = lt in
          ^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * int * y:int * z:int * int * int",
       but it is missing a component with label "z".
       Hint: use .. to ignore some components.
|}]

(* Full match, extra label *)
let matches =
  let ~x, k1, ~y, ~z, ~w, k2, k3 = lt in
  ()
[%%expect{|
Line 2, characters 6-32:
2 |   let ~x, k1, ~y, ~z, ~w, k2, k3 = lt in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * int * y:int * z:int * int * int",
       but it contains an extra component with label "w".
|}]

(* Full match, extra unlabeled label *)
let matches =
  let ~x, k1, ~y, ~z, k2, k3, k4 = lt in
  x, y, z
[%%expect{|
Line 2, characters 6-32:
2 |   let ~x, k1, ~y, ~z, k2, k3, k4 = lt in
          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * int * y:int * z:int * int * int",
       but it contains an extra unlabeled component.
|}]

(* Partial match *)
let matches =
  let ~x, ~z, .. = lt in
  x, z
[%%expect{|
val matches : int * int = (1, 4)
|}]

(* Partial match, reordered *)
let matches =
  let ~z, ~x, .. = lt in
  x, z
[%%expect{|
val matches : int * int = (1, 4)
|}]

(* Partial match, reordered, over-bound *)
let matches =
  let ~z:x, ~x, .. = lt in
  x
[%%expect{|
Line 2, characters 9-10:
2 |   let ~z:x, ~x, .. = lt in
             ^
Error: Variable "x" is bound several times in this matching
|}]

(* Partial match one *)
let matches =
  let ~z, .. = lt in
  z
[%%expect{|
val matches : int = 4
|}]

(* Partial match all *)
let matches =
   let ~x, k1, ~y, ~z, k2, k3, .. = lt in
   x, k1, y, z, k2, k3
[%%expect{|
Line 2, characters 7-33:
2 |    let ~x, k1, ~y, ~z, k2, k3, .. = lt in
           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 75 [unnecessarily-partial-tuple-pattern]: This tuple pattern
  unnecessarily ends in "..", as it explicitly matches all components of its
  expected type.

val matches : int * int * int * int * int * int = (1, 2, 3, 4, 5, 6)
|}]

(* Partial match bad name *)
let matches =
   let ~w, ~y, ~x, .. = lt in
   ()
[%%expect{|
Line 2, characters 7-21:
2 |    let ~w, ~y, ~x, .. = lt in
           ^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * int * y:int * z:int * int * int",
       but it contains an extra component with label "w".
|}]

(* Nested pattern *)
let f (z, (~y, ~x)) = x, y, z
[%%expect{|
val f : 'a * (y:'b * x:'c) -> 'c * 'b * 'a = <fun>
|}]

(* Non-principally known patterns *)

let f (z, (~y, ~x, ..)) = x, y, z
[%%expect{|
Line 1, characters 10-22:
1 | let f (z, (~y, ~x, ..)) = x, y, z
              ^^^^^^^^^^^^
Error: Could not determine the type of this partial tuple pattern.
|}]

let f (~x, ~y, ..) = x, y
[%%expect{|
Line 1, characters 6-18:
1 | let f (~x, ~y, ..) = x, y
          ^^^^^^^^^^^^
Error: Could not determine the type of this partial tuple pattern.
|}]

(* Labeled tuples nested in records *)

let x = ref (~x:1, ~y:2, 3, ~z:4)

(* Good match *)
let _1234 = match x with
| { contents = ~x:x0, ~y, w, ~z:x } -> x0, y, w, x
[%%expect{|
val x : (x:int * y:int * int * z:int) ref =
  {contents = (~x:1, ~y:2, 3, ~z:4)}
val _1234 : int * int * int * int = (1, 2, 3, 4)
|}]

(* Good partial match *)
let _1  = match x with
| { contents = ~y, ..} -> y
[%%expect{|
val _1 : int = 2
|}]

(* Wrong label *)
let () = match x with
| { contents = ~w , .. } -> w
[%%expect{|
Line 2, characters 15-22:
2 | | { contents = ~w , .. } -> w
                   ^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int",
       but it contains an extra component with label "w".
|}]

(* Missing unlabeled element *)
let () = match x with
| { contents = ~x, ~y, ~z } -> y
[%%expect{|
Line 2, characters 15-25:
2 | | { contents = ~x, ~y, ~z } -> y
                   ^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int", but it is missing an unlabeled component.
|}]

(* Extra unlabeled element *)
let () = match x with
| { contents = ~x, ~y, w1, ~z, w2 } -> y
[%%expect{|
Line 2, characters 15-33:
2 | | { contents = ~x, ~y, w1, ~z, w2 } -> y
                   ^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int",
       but it contains an extra unlabeled component.
|}]

(* Extra unlabeled element, open *)
let () = match x with
| { contents = ~x, ~y, w1, ~z, w2, .. } -> y
[%%expect{|
Line 2, characters 15-37:
2 | | { contents = ~x, ~y, w1, ~z, w2, .. } -> y
                   ^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int",
       but it contains an extra unlabeled component.
|}]

(* Missing label *)
let () = match x with
| { contents = ~x, ~y, w } -> y
[%%expect{|
Line 2, characters 15-24:
2 | | { contents = ~x, ~y, w } -> y
                   ^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int",
       but it is missing a component with label "z".
       Hint: use .. to ignore some components.
|}]

(* Extra label *)
let () = match x with
| { contents = ~z, ~y, ~w, ~x } -> y
[%%expect{|
Line 2, characters 15-29:
2 | | { contents = ~z, ~y, ~w, ~x } -> y
                   ^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       "x:int * y:int * int * z:int", but it is missing an unlabeled component.
|}]

(* Behavior w.r.t whether types are principally known *)

let f (z : (x:_ * y:_)) =
  match z with
  | ~y, ~x -> x + y
[%%expect{|
val f : (x:int * y:int) -> int = <fun>
|}]

let f = function ~x, ~y -> x + y

let g z =
  (f z, match z with ~y, ~x -> x + y)
[%%expect{|
val f : (x:int * y:int) -> int = <fun>
val g : (x:int * y:int) -> int * int = <fun>
|}, Principal{|
val f : (x:int * y:int) -> int = <fun>
Line 4, characters 21-27:
4 |   (f z, match z with ~y, ~x -> x + y)
                         ^^^^^^
Error: This pattern matches values of type "y:'a * x:'b"
       but a pattern was expected which matches values of type "x:int * y:int"
|}]

let f = function ~x, ~y -> x + y

let g z =
  match z with ~y, ~x -> x + y, f z
[%%expect{|
val f : (x:int * y:int) -> int = <fun>
Line 4, characters 34-35:
4 |   match z with ~y, ~x -> x + y, f z
                                      ^
Error: The value "z" has type "y:int * x:int"
       but an expression was expected of type "x:int * y:int"
|}]

(* More re-ordering stress tests *)
type t =
  x1:int *
  y2:int *
  int *
  x4:int *
  x5:int *
  y6:int *
  y7:int *
  int *
  int *
  y10:int *
  x11:int

let t : t = ~x1:1, ~y2:2, 3, ~x4:4, ~x5:5, ~y6:6, ~y7:7, 8, 9, ~y10:10, ~x11:11

let _ =
  let (~y2, ~y7, ~y10, ..) = t in
  y2, y7, y10
[%%expect{|
type t =
    x1:int * y2:int * int * x4:int * x5:int * y6:int * y7:int * int *
    int * y10:int * x11:int
val t : t =
  (~x1:1, ~y2:2, 3, ~x4:4, ~x5:5, ~y6:6, ~y7:7, 8, 9, ~y10:10, ~x11:11)
- : int * int * int = (2, 7, 10)
|}]

let _ =
  let (a, b, c, ..) = t in
  (a, b, c)
[%%expect{|
- : int * int * int = (3, 8, 9)
|}]

let _ =
  let (n3, ~y6:n6, ~y7, ~x1:n1, ..) = t in
  (n1, n6, n3, y7)
[%%expect{|
- : int * int * int * int = (1, 6, 3, 7)
|}]

let _ =
  let (~x4, ~x1, ~x11, ~x5, ..) = t in
  (x1, x4, x5, x11)
[%%expect{|
- : int * int * int * int = (1, 4, 5, 11)
|}]

let _ =
  let (~y2:n2, ~y6:n6, n3, ~x1:n1, ~y7:n7, n8,
       ~y10:n10, ~x4:n4, ~x5:n5, ~x11:n11, n9) =
    t
  in
  (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11)
[%%expect{|
- : int * int * int * int * int * int * int * int * int * int * int =
(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
|}]

let _ =
  let (n3, n8, n9, ~y2:n2, ~y6:n6, ~y7:n7,
       ~y10:n10, ~x1:n1, ~x4:n4, ~x5:n5, ~x11:n11) =
    t
  in
  (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11)
[%%expect{|
- : int * int * int * int * int * int * int * int * int * int * int =
(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
|}]

let _ =
  let (~x1:n1, ~y2:n2, n3, ~x4:n4, ~x5:n5,
       ~y6:n6, ~y7:n7, n8, n9, ~y10:n10, ~x11:n11) =
    t
  in
  (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11)
[%%expect{|
- : int * int * int * int * int * int * int * int * int * int * int =
(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
|}]
