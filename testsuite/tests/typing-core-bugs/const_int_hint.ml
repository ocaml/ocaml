(* TEST
   * expect
*)

let _ = Int32.(add 1 2l);;
[%%expect{|
Line 1, characters 19-20:
1 | let _ = Int32.(add 1 2l);;
                       ^
Error: This expression has type int but an expression was expected of type
         int32
  Hint: Did you mean `1l'?
|}]

let _ : int32 * int32 = 42l, 43;;
[%%expect{|
Line 1, characters 29-31:
1 | let _ : int32 * int32 = 42l, 43;;
                                 ^^
Error: This expression has type int but an expression was expected of type
         int32
  Hint: Did you mean `43l'?
|}]

let _ : int32 * nativeint = 42l, 43;;
[%%expect{|
Line 1, characters 33-35:
1 | let _ : int32 * nativeint = 42l, 43;;
                                     ^^
Error: This expression has type int but an expression was expected of type
         nativeint
  Hint: Did you mean `43n'?
|}]

let _ = min 6L 7;;
[%%expect{|
Line 1, characters 15-16:
1 | let _ = min 6L 7;;
                   ^
Error: This expression has type int but an expression was expected of type
         int64
  Hint: Did you mean `7L'?
|}]

let _ : float = 123;;
[%%expect{|
Line 1, characters 16-19:
1 | let _ : float = 123;;
                    ^^^
Error: This expression has type int but an expression was expected of type
         float
  Hint: Did you mean `123.'?
|}]

(* no hint *)
let x = 0
let _ = Int32.(add x 2l);;
[%%expect{|
val x : int = 0
Line 2, characters 19-20:
2 | let _ = Int32.(add x 2l);;
                       ^
Error: This expression has type int but an expression was expected of type
         int32
|}]

(* pattern *)
let _ : int32 -> int32 = function
  | 0 -> 0l
  | x -> x
[%%expect{|
Line 2, characters 4-5:
2 |   | 0 -> 0l
        ^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type int32
  Hint: Did you mean `0l'?
|}]

let _ : int64 -> int64 = function
  | 1L | 2 -> 3L
  | x -> x;;
[%%expect{|
Line 2, characters 9-10:
2 |   | 1L | 2 -> 3L
             ^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type int64
  Hint: Did you mean `2L'?
|}]

(* symmetric *)
let _ : int32 = 1L;;
[%%expect{|
Line 1, characters 16-18:
1 | let _ : int32 = 1L;;
                    ^^
Error: This expression has type int64 but an expression was expected of type
         int32
  Hint: Did you mean `1l'?
|}]
let _ : float = 1L;;
[%%expect{|
Line 1, characters 16-18:
1 | let _ : float = 1L;;
                    ^^
Error: This expression has type int64 but an expression was expected of type
         float
  Hint: Did you mean `1.'?
|}]
let _ : int64 = 1n;;
[%%expect{|
Line 1, characters 16-18:
1 | let _ : int64 = 1n;;
                    ^^
Error: This expression has type nativeint
       but an expression was expected of type int64
  Hint: Did you mean `1L'?
|}]
let _ : nativeint = 1l;;
[%%expect{|
Line 1, characters 20-22:
1 | let _ : nativeint = 1l;;
                        ^^
Error: This expression has type int32 but an expression was expected of type
         nativeint
  Hint: Did you mean `1n'?
|}]

(* not implemented *)
let _ : int64 = 0.;;
[%%expect{|
Line 1, characters 16-18:
1 | let _ : int64 = 0.;;
                    ^^
Error: This expression has type float but an expression was expected of type
         int64
|}]
let _ : int = 1L;;
[%%expect{|
Line 1, characters 14-16:
1 | let _ : int = 1L;;
                  ^^
Error: This expression has type int64 but an expression was expected of type
         int
|}]

(* Check that the hint preserves formatting of int, int32, int64 and nativeint
   literals in decimal, hexadecimal, octal and binary notation *)
let _ : int64 = min 0L 1_000;;
[%%expect{|
Line 1, characters 23-28:
1 | let _ : int64 = min 0L 1_000;;
                           ^^^^^
Error: This expression has type int but an expression was expected of type
         int64
  Hint: Did you mean `1_000L'?
|}]
let _ : nativeint * nativeint = 0n, 0xAA_BBL;;
[%%expect{|
Line 1, characters 36-44:
1 | let _ : nativeint * nativeint = 0n, 0xAA_BBL;;
                                        ^^^^^^^^
Error: This expression has type int64 but an expression was expected of type
         nativeint
  Hint: Did you mean `0xAA_BBn'?
|}]
let _ : int32 -> int32 = function
  | 1l | 0o2_345 -> 3l
  | x -> x;;
[%%expect{|
Line 2, characters 9-16:
2 |   | 1l | 0o2_345 -> 3l
             ^^^^^^^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type int32
  Hint: Did you mean `0o2_345l'?
|}]
let _ : int32 -> int32 = fun x -> match x with
  | 1l | 0b1000_1101 -> 3l
  | x -> x;;
[%%expect{|
Line 2, characters 9-20:
2 |   | 1l | 0b1000_1101 -> 3l
             ^^^^^^^^^^^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type int32
  Hint: Did you mean `0b1000_1101l'?
|}]
type t1 = {f1: int32};; let _ = fun x -> x.f1 <- 1_000n;;
[%%expect{|
type t1 = { f1 : int32; }
Line 1, characters 49-55:
1 | type t1 = {f1: int32};; let _ = fun x -> x.f1 <- 1_000n;;
                                                     ^^^^^^
Error: This expression has type nativeint
       but an expression was expected of type int32
  Hint: Did you mean `1_000l'?
|}]
