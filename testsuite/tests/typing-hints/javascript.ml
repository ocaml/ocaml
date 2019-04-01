(* TEST
   * expect
*)

(* Injectivity *)

let _ =
  let a = 42 and b = "42"
  in
    a+b
;;
[%%expect{|
Line 4, characters 6-7:
4 |     a+b
          ^
Error: This expression has type string but an expression was expected of type
         int
Hint: Did you mean to use `Obj.magic' ?
|}];;
