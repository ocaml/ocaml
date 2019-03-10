(* TEST
   * expect
*)

let _ = 0. + 0.
[%%expect{|
Line 1, characters 8-10:
1 | let _ = 0. + 0.
            ^^
Error: This expression has type float but an expression was expected of type
         int
Line 1, characters 11-12:
1 | let _ = 0. + 0.
               ^
  Hint: Did you mean to use `+.'?
|}]

let _ = 0l - 0l
[%%expect{|
Line 1, characters 8-10:
1 | let _ = 0l - 0l
            ^^
Error: This expression has type int32 but an expression was expected of type
         int
Line 1, characters 11-12:
1 | let _ = 0l - 0l
               ^
  Hint: Did you mean to use `Int32.sub'?
|}]

let _ = 0L * 0L
[%%expect{|
Line 1, characters 8-10:
1 | let _ = 0L * 0L
            ^^
Error: This expression has type int64 but an expression was expected of type
         int
Line 1, characters 11-12:
1 | let _ = 0L * 0L
               ^
  Hint: Did you mean to use `Int64.mul'?
|}]

let _ = 0n / 0n
[%%expect{|
Line 1, characters 8-10:
1 | let _ = 0n / 0n
            ^^
Error: This expression has type nativeint
       but an expression was expected of type int
Line 1, characters 11-12:
1 | let _ = 0n / 0n
               ^
  Hint: Did you mean to use `Nativeint.div'?
|}]

let _ = 0. mod 0.
[%%expect{|
Line 1, characters 8-10:
1 | let _ = 0. mod 0.
            ^^
Error: This expression has type float but an expression was expected of type
         int
Line 1, characters 11-14:
1 | let _ = 0. mod 0.
               ^^^
  Hint: Did you mean to use `Float.rem'?
|}]

(* disabled *)
let _ = 0 +. 0
[%%expect{|
Line 1, characters 8-9:
1 | let _ = 0 +. 0
            ^
Error: This expression has type int but an expression was expected of type
         float
       Hint: Did you mean `0.'?
|}]
