(* TEST
   expect;
*)

(* This test showcases the various denominations used in type clash errors *)

let f (x : < m : float >) = print_int x#m

[%%expect {|
Line 1, characters 38-41:
1 | let f (x : < m : float >) = print_int x#m
                                          ^^^
Error: The method call "x#m" has type "float"
       but an expression was expected of type "int"
|}]

type r = { f : float }

let f (x : r) = print_int x.f

[%%expect {|
type r = { f : float; }
Line 3, characters 26-29:
3 | let f (x : r) = print_int x.f
                              ^^^
Error: The field access "x.f" has type "float"
       but an expression was expected of type "int"
|}]

type v = Cons

let _ = print_int Cons

[%%expect {|
type v = Cons
Line 3, characters 18-22:
3 | let _ = print_int Cons
                      ^^^^
Error: The constructor "Cons" has type "v" but an expression was expected of type
         "int"
|}]

let _ = print_int `Cons

[%%expect {|
Line 1, characters 18-23:
1 | let _ = print_int `Cons
                      ^^^^^
Error: The constructor "`Cons" has type "[> `Cons ]"
       but an expression was expected of type "int"
|}]

let v = 0.
let _ = print_int v

[%%expect {|
val v : float = 0.
Line 2, characters 18-19:
2 | let _ = print_int v
                      ^
Error: The value "v" has type "float" but an expression was expected of type "int"
|}]

let _ = print_int 0.

[%%expect {|
Line 1, characters 18-20:
1 | let _ = print_int 0.
                      ^^
Error: The constant "0." has type "float" but an expression was expected of type
         "int"
|}]

let _ = print_int "foo"

[%%expect {|
Line 1, characters 18-23:
1 | let _ = print_int "foo"
                      ^^^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

let _ : int = while false do () done

[%%expect {|
Line 1, characters 14-36:
1 | let _ : int = while false do () done
                  ^^^^^^^^^^^^^^^^^^^^^^
Error: This "while" expression has type "unit"
       but an expression was expected of type "int"
|}]

let _ : int = for _ = 1 to 2 do () done

[%%expect {|
Line 1, characters 14-39:
1 | let _ : int = for _ = 1 to 2 do () done
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "for" expression has type "unit"
       but an expression was expected of type "int"
|}]
