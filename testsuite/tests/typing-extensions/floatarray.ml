(* TEST
   * expect
*)

let x = [|1.;2.|]
;;
[%%expect {|
val x : float array = [|1.; 2.|]
|}]

let x : floatarray = [|1.; 2.|]
;;
[%%expect {|
val x : floatarray = [|1.; 2.|]
|}]

let x = ([|1.; 2.|] : floatarray)
;;
[%%expect {|
val x : floatarray = [|1.; 2.|]
|}]

let f (a : floatarray) = match a with [|x|] -> x | _ -> assert false
;;
[%%expect {|
val f : floatarray -> float = <fun>
|}]

let _ = f [|1.|]
;;
[%%expect {|
- : float = 1.
|}]

(* Does not work without the annotation *)

let f a = match a with [|_|] -> Float.Array.length a | _ -> assert false
;;
[%%expect {|
Line 1, characters 51-52:
1 | let f a = match a with [|_|] -> Float.Array.length a | _ -> assert false
                                                       ^
Error: This expression has type 'a array
       but an expression was expected of type Float.Array.t = floatarray
|}]

type s = floatarray
type t = s
let x : t = [||]
;;
[%%expect {|
type s = floatarray
type t = s
val x : t = [||]
|}]

let f a =
  let _ = Float.Array.length a in
  match a with
  | [||] -> ()
  | _ -> ()
;;
[%%expect{|
val f : Float.Array.t -> unit = <fun>
|}, Principal{|
Line 4, characters 4-8:
4 |   | [||] -> ()
        ^^^^
Warning 18 [not-principal]: this type-based field disambiguation is not principal.
val f : Float.Array.t -> unit = <fun>
|}]

[@@@warning "+a"]
;;

let _ = ([||] : floatarray)

[%%expect{|
Line 4, characters 9-13:
4 | let _ = ([||] : floatarray)
             ^^^^
Warning 42 [disambiguated-name]: this use of ([||]) relies on type-directed disambiguation,
it will not compile with OCaml 4.00 or earlier.
- : floatarray = [||]
|}]
