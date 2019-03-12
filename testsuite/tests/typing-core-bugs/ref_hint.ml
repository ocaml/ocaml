(* TEST
   * expect
*)

let a = ref 0
let _ = a + 1
[%%expect{|
val a : int ref = {contents = 0}
Line 2, characters 8-9:
2 | let _ = a + 1
            ^
Error: This expression has type int ref
       but an expression was expected of type int
Hint: This is a `ref', did you mean `!a'?
|}]

let b () = ref 0
let _ = b () + 1
[%%expect{|
val b : unit -> int ref = <fun>
Line 2, characters 8-12:
2 | let _ = b () + 1
            ^^^^
Error: This expression has type int ref
       but an expression was expected of type int
Hint: This is a `ref', did you mean `!( .. )'?
|}]

(* corner cases *)
let b = ref 0
let _ =
  let (!) x = x + 1 in
  b + 1
[%%expect{|
val b : int ref = {contents = 0}
Line 4, characters 2-3:
4 |   b + 1
      ^
Error: This expression has type int ref
       but an expression was expected of type int
Hint: This is a `ref', did you mean `Stdlib.(!) b'?
|}]

type t = { x : int }
let _ = { x = 3; contents = 0 }
[%%expect{|
type t = { x : int; }
Line 2, characters 17-25:
2 | let _ = { x = 3; contents = 0 }
                     ^^^^^^^^
Error: The record field contents belongs to the type 'a ref
       but is mixed here with fields of type t
|}]

type 'a ref = Not_ref of 'a
let _ = (Not_ref 0) + 0
[%%expect{|
type 'a ref = Not_ref of 'a
Line 2, characters 8-19:
2 | let _ = (Not_ref 0) + 0
            ^^^^^^^^^^^
Error: This expression has type 'a ref but an expression was expected of type
         int
|}]

(* limitation *)
let f r =
  ignore !r;
  r + 1
[%%expect{|
Line 3, characters 2-3:
3 |   r + 1
      ^
Error: This expression has type 'a Stdlib.ref
       but an expression was expected of type int
|}]
