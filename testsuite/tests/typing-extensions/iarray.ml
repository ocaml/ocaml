(* TEST
 expect;
*)

(** Create some immutable and mutable arrays *)

let iarray  : int   iarray = [|1;2;3;4;5|];;
let ifarray : float iarray = [|1.5;2.5;3.5;4.5;5.5|];;

let marray  : int   array = [|1;2;3;4;5|];;
let mfarray : float array = [|1.5;2.5;3.5;4.5;5.5|];;

[%%expect{|
val iarray : int iarray = [|1; 2; 3; 4; 5|]
val ifarray : float iarray = [|1.5; 2.5; 3.5; 4.5; 5.5|]
val marray : int array = [|1; 2; 3; 4; 5|]
val mfarray : float array = [|1.5; 2.5; 3.5; 4.5; 5.5|]
|}];;

(** Pattern-match on some immutable arrays, and check the typing of array
    patterns, both mutable and immutable *)

match iarray with
| [||]          -> "empty"
| [|1;2;3;4;5|] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "1--5"
|}];;

match ifarray with
| [||]                    -> "empty"
| [|1.5;2.5;3.5;4.5;5.5|] -> "1.5--5.5"
| _                       -> "who knows?"
;;
[%%expect{|
- : string = "1.5--5.5"
|}];;

match iarray with
| [||]          -> "empty"
| [|1;2;3;4;6|] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "who knows?"
|}];;

match ifarray with
| [||]          -> "empty"
| [|1;2;3;4;5|] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 3, characters 4-5:
3 | | [|1;2;3;4;5|] -> "1--5"
        ^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "float"
  Hint: Did you mean "1."?
|}];;

match marray with
| ([||] : _ iarray) -> "empty"
| [|1;2;3;4;5|]     -> "1--5"
| _                 -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-19:
2 | | ([||] : _ iarray) -> "empty"
      ^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "'a iarray"
       but a pattern was expected which matches values of type "int array"
|}];;

match iarray with
| ([||] : _ array) -> "empty"
| [|1;2;3;4;5|]    -> "1--5"
| _                -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-18:
2 | | ([||] : _ array) -> "empty"
      ^^^^^^^^^^^^^^^^
Error: This pattern matches values of type "'a array"
       but a pattern was expected which matches values of type "int iarray"
|}];;
