(* TEST
   * expect
*)

let rec x = (x; ());;
[%%expect{|
val x : unit = ()
|}];;

let rec x = let x = () in x;;
[%%expect{|
val x : unit = ()
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

let rec x = let module M = struct let f = x end in ();;
[%%expect{|
val x : unit = ()
|}];;

module type T = sig val y: int end

let rec x = let module M =
            struct
              module N =
              struct
                let y = x
              end
            end
  in fun () -> ignore (M.N.y ());;
[%%expect{|
module type T = sig val y : int end
val x : unit -> unit = <fun>
|}];;

let rec x = "x";;
[%%expect{|
val x : string = "x"
|}];;

class c = object end
let rec x = fun () -> new c;;
[%%expect{|
class c : object  end
val x : unit -> c = <fun>
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

let rec x = lazy (Lazy.force x + Lazy.force x)
  ;;
[%%expect{|
val x : int Lazy.t = <lazy>
|}];;

let rec x = { x with contents = 3 }  [@ocaml.warning "-23"];;
[%%expect{|
val x : int ref = {contents = 3}
|}];;

let rec x = let y = (x; ()) in y;;
[%%expect{|
val x : unit = ()
|}];;

let rec x = [|y|] and y = 0;;
[%%expect{|
val x : int array = [|0|]
val y : int = 0
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

(* This test is not allowed if 'a' is unboxed, but should be accepted
   as written *)
type a = {a: b}
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;
[%%expect{|
type a = { a : b; }
and b = X of a | Y
val a : a = {a = X <cycle>}
|}];;

(* This test is not allowed if 'c' is unboxed, but should be accepted
   as written *)
type d = D of e
and e = V of d | W;;
[%%expect{|
type d = D of e
and e = V of d | W
|}];;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;
[%%expect{|
val d : d = D (V <cycle>)
|}];;

type r = R of r list [@@unboxed]
let rec a = R [a];;
[%%expect{|
type r = R of r list [@@unboxed]
val a : r = R [<cycle>]
|}];;
