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

(* Check that the hint preserves formatting for int32 literals *)
let _ = Int64.(add 1_000l 2L);;
[%%expect{|
Line 1, characters 19-25:
1 | let _ = Int64.(add 1_000l 2L);;
                       ^^^^^^
Error: This expression has type int32 but an expression was expected of type
         int64
  Hint: Did you mean `1_000L'?
|}]

let _ = Int64.(add 0xAA_BBl 2L);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int64.(add 0xAA_BBl 2L);;
                       ^^^^^^^^
Error: This expression has type int32 but an expression was expected of type
         int64
  Hint: Did you mean `0xAA_BBL'?
|}]

let _ = Int64.(add 0o2_345l 2L);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int64.(add 0o2_345l 2L);;
                       ^^^^^^^^
Error: This expression has type int32 but an expression was expected of type
         int64
  Hint: Did you mean `0o2_345L'?
|}]

let _ = Int64.(add 0b1000_1101l 2L);;
[%%expect{|
Line 1, characters 19-31:
1 | let _ = Int64.(add 0b1000_1101l 2L);;
                       ^^^^^^^^^^^^
Error: This expression has type int32 but an expression was expected of type
         int64
  Hint: Did you mean `0b1000_1101L'?
|}]

(* Check that the hint preserves formatting for int64 literals *)
let _ = Int32.(add 1_000L 2l);;
[%%expect{|
Line 1, characters 19-25:
1 | let _ = Int32.(add 1_000L 2l);;
                       ^^^^^^
Error: This expression has type int64 but an expression was expected of type
         int32
  Hint: Did you mean `1_000l'?
|}]

let _ = Int32.(add 0xAA_BBL 2l);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int32.(add 0xAA_BBL 2l);;
                       ^^^^^^^^
Error: This expression has type int64 but an expression was expected of type
         int32
  Hint: Did you mean `0xAA_BBl'?
|}]

let _ = Int32.(add 0o2_345L 2l);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int32.(add 0o2_345L 2l);;
                       ^^^^^^^^
Error: This expression has type int64 but an expression was expected of type
         int32
  Hint: Did you mean `0o2_345l'?
|}]

let _ = Int32.(add 0b1000_1101L 2l);;
[%%expect{|
Line 1, characters 19-31:
1 | let _ = Int32.(add 0b1000_1101L 2l);;
                       ^^^^^^^^^^^^
Error: This expression has type int64 but an expression was expected of type
         int32
  Hint: Did you mean `0b1000_1101l'?
|}]

(* Check that the hint preserves formatting for nativeint literals *)
let _ = Int64.(add 1_000n 2L);;
[%%expect{|
Line 1, characters 19-25:
1 | let _ = Int64.(add 1_000n 2L);;
                       ^^^^^^
Error: This expression has type nativeint
       but an expression was expected of type int64
  Hint: Did you mean `1_000L'?
|}]

let _ = Int64.(add 0xAA_BBn 2L);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int64.(add 0xAA_BBn 2L);;
                       ^^^^^^^^
Error: This expression has type nativeint
       but an expression was expected of type int64
  Hint: Did you mean `0xAA_BBL'?
|}]

let _ = Int64.(add 0o2_345n 2L);;
[%%expect{|
Line 1, characters 19-27:
1 | let _ = Int64.(add 0o2_345n 2L);;
                       ^^^^^^^^
Error: This expression has type nativeint
       but an expression was expected of type int64
  Hint: Did you mean `0o2_345L'?
|}]

let _ = Int64.(add 0b1000_1101n 2L);;
[%%expect{|
Line 1, characters 19-31:
1 | let _ = Int64.(add 0b1000_1101n 2L);;
                       ^^^^^^^^^^^^
Error: This expression has type nativeint
       but an expression was expected of type int64
  Hint: Did you mean `0b1000_1101L'?
|}]
