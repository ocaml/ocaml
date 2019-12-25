(* TEST
   * expect
*)

let rec x = (x; ());;
[%%expect{|
val x : unit = ()
|}];;

let rec x = "x";;
[%%expect{|
val x : string = "x"
|}];;

let rec x = let x = () in x;;
[%%expect{|
val x : unit = ()
|}];;

let rec x = let y = (x; ()) in y;;
[%%expect{|
val x : unit = ()
|}];;

let rec x = let y = () in x;;
[%%expect{|
Line 1, characters 12-27:
1 | let rec x = let y = () in x;;
                ^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = [y]
and y = let x = () in x;;
[%%expect{|
val x : unit list = [()]
val y : unit = ()
|}];;

let rec x = [y]
and y = let rec x = () in x;;
[%%expect{|
val x : unit list = [()]
val y : unit = ()
|}];;

let rec x =
  let a = x in
  fun () -> a ()
and y =
  [x];;
[%%expect{|
val x : unit -> 'a = <fun>
val y : (unit -> 'a) list = [<fun>]
|}];;

let rec x = [|y|] and y = 0;;
[%%expect{|
val x : int array = [|0|]
val y : int = 0
|}];;


let rec x = (y, y)
and y = fun () -> ignore x;;
[%%expect{|
val x : (unit -> unit) * (unit -> unit) = (<fun>, <fun>)
val y : unit -> unit = <fun>
|}];;

let rec x = Some y
and y = fun () -> ignore x
;;
[%%expect{|
val x : (unit -> unit) option = Some <fun>
val y : unit -> unit = <fun>
|}];;

let rec x = ignore x;;
[%%expect{|
Line 1, characters 12-20:
1 | let rec x = ignore x;;
                ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = y 0 and y _ = ();;
[%%expect{|
Line 1, characters 12-15:
1 | let rec x = y 0 and y _ = ();;
                ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec b = if b then true else false;;
[%%expect{|
Line 1, characters 12-37:
1 | let rec b = if b then true else false;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = function
    Some _ -> ignore (y [])
  | None -> ignore (y [])
and y = function
    [] -> ignore (x None)
  | _ :: _ -> ignore (x None)
    ;;
[%%expect{|
val x : 'a option -> unit = <fun>
val y : 'a list -> unit = <fun>
|}];;

(* this is accepted as all fields are overridden *)
let rec x = { x with contents = 3 }  [@ocaml.warning "-23"];;
[%%expect{|
val x : int ref = {contents = 3}
|}];;

(* this is rejected as `c` will be dereferenced during the copy,
   and is not yet fully defined *)
let rec c = { c with Complex.re = 1.0 };;
[%%expect{|
Line 1, characters 12-39:
1 | let rec c = { c with Complex.re = 1.0 };;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = `A y
and y = fun () -> ignore x
;;
[%%expect{|
val x : [> `A of unit -> unit ] = `A <fun>
val y : unit -> unit = <fun>
|}];;

let rec x = { contents = y }
and y = fun () -> ignore x;;
[%%expect{|
val x : (unit -> unit) ref = {contents = <fun>}
val y : unit -> unit = <fun>
|}];;

let r = ref (fun () -> ())
let rec x = fun () -> r := x;;
[%%expect{|
val r : (unit -> unit) ref = {contents = <fun>}
val x : unit -> unit = <fun>
|}];;

let rec x = fun () -> y.contents and y = { contents = 3 };;
[%%expect{|
val x : unit -> int = <fun>
val y : int ref = {contents = 3}
|}];;

let r = ref ()
let rec x = r := x;;
[%%expect{|
val r : unit ref = {contents = ()}
Line 2, characters 12-18:
2 | let rec x = r := x;;
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = 0 to 1 do
    let z = y in ignore z
  done
and y = x; ();;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..for i = 0 to 1 do
3 |     let z = y in ignore z
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = 0 to y do
    ()
  done
and y = 10;;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..for i = 0 to y do
3 |     ()
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  for i = y to 10 do
    ()
  done
and y = 0;;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..for i = y to 10 do
3 |     ()
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while false do
    let y = x in ignore y
  done
and y = x; ();;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..while false do
3 |     let y = x in ignore y
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while y do
    ()
  done
and y = false;;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..while y do
3 |     ()
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  while y do
    let y = x in ignore y
  done
and y = false;;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..while y do
3 |     let y = x in ignore y
4 |   done
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;



let rec x = y.contents and y = { contents = 3 };;
[%%expect{|
Line 1, characters 12-22:
1 | let rec x = y.contents and y = { contents = 3 };;
                ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = assert y and y = true;;
[%%expect{|
Line 1, characters 12-20:
1 | let rec x = assert y and y = true;;
                ^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* Recursively constructing arrays of known non-float type is permitted *)
let rec deep_cycle : [`Tuple of [`Shared of 'a] array] as 'a
  = `Tuple [| `Shared deep_cycle |];;
[%%expect{|
val deep_cycle : [ `Tuple of [ `Shared of 'a ] array ] as 'a =
  `Tuple [|`Shared <cycle>|]
|}];;

(* Constructing float arrays was disallowed altogether at one point
   by an overzealous check.  Constructing float arrays in recursive
   bindings is fine when they don't partake in the recursion. *)
let rec _x = let _ = [| 1.0 |] in 1. in ();;
[%%expect{|
- : unit = ()
|}];;

(* The builtin Stdlib.ref is currently treated as a constructor.
   Other functions of the same name should not be so treated. *)
let _ =
  let module Stdlib =
  struct
    let ref _ = assert false
  end in
  let rec x = Stdlib.ref y
  and y = fun () -> ignore x
  in (x, y)
;;
[%%expect{|
Line 6, characters 14-26:
6 |   let rec x = Stdlib.ref y
                  ^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

(* An example, from Leo White, of let rec bindings that allocate
   values of unknown size *)
let foo p x =
  let rec f =
    if p then (fun y -> x + g y) else (fun y -> g y)
  and g =
    if not p then (fun y -> x - f y) else (fun y -> f y)
  in
  (f, g)
;;
[%%expect{|
Line 3, characters 4-52:
3 |     if p then (fun y -> x + g y) else (fun y -> g y)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x =
  match let _ = y in raise Not_found with
    _ -> "x"
  | exception Not_found -> "z"
and y = match x with
  z -> ("y", z);;
[%%expect{|
Lines 2-4, characters 2-30:
2 | ..match let _ = y in raise Not_found with
3 |     _ -> "x"
4 |   | exception Not_found -> "z"
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;


(* To compute the dependencies of mutually-recursive bindings,
   transitive dependencies must be taken into account.

   The example below was causing a segfault in 4.08+dev.
*)
let rec wrong =
  (* x depends on y,
     and y depends on wrong,
     so it is important to notice that x transitively depends on wrong;

     an earlier version of our letrec analysis would only report that
     y depends on wrong, which seems safe as y is not used in the
     body.
  *)
  let rec x = ref y
  and y = ref wrong
  in ref ("foo" ^ ! ! !x);;
[%%expect{|
Lines 10-12, characters 2-25:
10 | ..let rec x = ref y
11 |   and y = ref wrong
12 |   in ref ("foo" ^ ! ! !x)..
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}]

(* in this case, x does not depend on y, so everything is fine *)
let rec okay =
  let rec x = ref "bar"
  and _y = ref okay in
  ref ("foo" ^ ! x);;
[%%expect{|
val okay : string ref = {contents = "foobar"}
|}]

let rec okay =
  let ok = okay in
  let rec x = ref "bar"
  and _y = ref ok
  and _z = ref x, ok
  in
  ref ("foo" ^ ! x);;
[%%expect{|
val okay : string ref = {contents = "foobar"}
|}]

let rec a = 0 :: b
and b = 1 :: a
and f =
  let x = a in
  let y = x in
  let z = y in
  fun i -> List.hd z + i
[%%expect{|
val a : int list = [0; 1; <cycle>]
val b : int list = [1; 0; <cycle>]
val f : int -> int = <fun>
|}]

let rec a = 0 :: b
and b = 1 :: a
and f =
  let rec x = a in
  let y = x in
  let z = y in
  fun i -> List.hd z + i
[%%expect{|
val a : int list = [0; 1; <cycle>]
val b : int list = [1; 0; <cycle>]
val f : int -> int = <fun>
|}]

let rec okay =
  let rec x = let r = "bar" in ref r
  and y = fun s -> ignore xx; ref s
  and _z = fun s -> y s
  and xx = let k = 0 in k::yy
  and yy = 1::xx
  in
  ref ("foo" ^ ! (y !x));;
[%%expect{|
val okay : string ref = {contents = "foobar"}
|}]
