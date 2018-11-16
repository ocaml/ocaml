(* TEST
   * expect
*)

class c = object end
let rec x = fun () -> new c;;
[%%expect{|
class c : object  end
val x : unit -> c = <fun>
|}];;

class c _ = object end
let rec x = new c x;;
[%%expect{|
class c : 'a -> object  end
Line 2, characters 12-19:
2 | let rec x = new c x;;
                ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = y#m and y = object method m = () end;;
[%%expect{|
Line 1, characters 12-15:
1 | let rec x = y#m and y = object method m = () end;;
                ^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = (object method m _ = () end)#m x;;
[%%expect{|
Line 1, characters 12-44:
1 | let rec x = (object method m _ = () end)#m x;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object val mutable v = 0 method m = v <- y end and y = 1;;
[%%expect{|
Line 1, characters 12-58:
1 | let rec x = object val mutable v = 0 method m = v <- y end and y = 1;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object method m = x end;;
[%%expect{|
Line 1, characters 12-35:
1 | let rec x = object method m = x end;;
                ^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = object method m = ignore x end;;
[%%expect{|
Line 1, characters 12-42:
1 | let rec x = object method m = ignore x end;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
