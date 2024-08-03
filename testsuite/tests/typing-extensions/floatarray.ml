(* TEST
   expect;
*)

let x = [|1.;2.|]
;;
[%%expect {|
val x : float array = [|1.; 2.|]
|}]

let x : floatarray = [|1.; 2.|]
;;
[%%expect {|
val x : floatarray = <abstr>
|}]

let x = ([|1.; 2.|] : floatarray)
;;
[%%expect {|
val x : floatarray = <abstr>
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
Error: The value "a" has type "'a array" but an expression was expected of type
         "Float.Array.t" = "floatarray"
|}]

type s = floatarray
type t = s
let x : t = [||]
;;
[%%expect {|
type s = floatarray
type t = s
val x : t = <abstr>
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
Error: This pattern matches values of type "'a array"
       but a pattern was expected which matches values of type
         "Float.Array.t" = "floatarray"
|}]
