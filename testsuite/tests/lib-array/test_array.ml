(* TEST
   * expect
*)

let a = Array.make 8 None;;
let _ = Array.fill a 2 3 (Some 42);;
a;;
[%%expect{|
val a : '_weak1 option array =
  [|None; None; None; None; None; None; None; None|]
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 42; Some 42; None; None; None|]
|}]
let _ = Array.fill a 3 1 (Some 0);;
a;;
[%%expect{|
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; None; None; None|]
|}]
let _ = Array.fill a 3 6 None;;
a;;
[%%expect{|
Exception: Invalid_argument "Array.fill".
|}]
let _ = Array.fill a (-1) 2 None;;
a;;
[%%expect{|
Exception: Invalid_argument "Array.fill".
|}]
let _ = Gc.compact ();;
let _ = Array.fill a 5 1 (Some (if Random.int 2 < 0 then 1 else 2));;
a;;
[%%expect{|
- : unit = ()
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; Some 2; None; None|]
|}]
let _ = Array.fill a 5 1 None;;
a;;
[%%expect{|
- : unit = ()
- : int option array =
[|None; None; Some 42; Some 0; Some 42; None; None; None|]
|}]


let a = Array.make 8 0.;;
let _ = Array.fill a 2 3 42.;;
a;;
[%%expect{|
val a : float array = [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|]
- : unit = ()
- : float array = [|0.; 0.; 42.; 42.; 42.; 0.; 0.; 0.|]
|}]
