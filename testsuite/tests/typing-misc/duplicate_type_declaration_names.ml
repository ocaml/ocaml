(* TEST
   expect;
*)

type constructor_duplicate_two =
  | X
  | X
;;

[%% expect {|
Line 2, characters 4-5:
2 |   | X
        ^
Error: Two constructors are named "X"
Line 3, characters 4-5:
3 |   | X
        ^
  Duplicate definition here
|}]


type constructor_duplicate_multiple =
  | X
  | X
  | X
;;

[%% expect {|
Line 2, characters 4-5:
2 |   | X
        ^
Error: Multiple constructors are named "X"
Line 3, characters 4-5:
3 |   | X
        ^
  Duplicate definition here
Line 4, characters 4-5:
4 |   | X
        ^
  Duplicate definition here
|}]

type label_duplicate_two = {
  x: unit;
  x: unit;
};;

[%% expect {|
Line 2, characters 2-3:
2 |   x: unit;
      ^
Error: Two labels are named "x"
Line 3, characters 2-3:
3 |   x: unit;
      ^
  Duplicate definition here
|}]


type label_duplicate_multiple = {
  x: unit;
  x: unit;
  x: unit;
};;

[%% expect {|
Line 2, characters 2-3:
2 |   x: unit;
      ^
Error: Multiple labels are named "x"
Line 3, characters 2-3:
3 |   x: unit;
      ^
  Duplicate definition here
Line 4, characters 2-3:
4 |   x: unit;
      ^
  Duplicate definition here
|}]
