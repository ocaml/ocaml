(* TEST
   expect;
*)

type constructor_duplicate_two =
  | X
  | X
;;

[%% expect {|
Lines 1-3, characters 0-5:
1 | type constructor_duplicate_two =
2 |   | X
3 |   | X
Error: Two constructors are named "X"
Line 2, characters 4-5:
2 |   | X
        ^
  First definition was here
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
Lines 1-4, characters 0-5:
1 | type constructor_duplicate_multiple =
2 |   | X
3 |   | X
4 |   | X
Error: Multiple constructors are named "X"
Line 2, characters 4-5:
2 |   | X
        ^
  First definition was here
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
Line 3, characters 2-3:
3 |   x: unit;
      ^
Error: Two labels are named "x"
Line 2, characters 2-3:
2 |   x: unit;
      ^
  First definition was here
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
Line 4, characters 2-3:
4 |   x: unit;
      ^
Error: Multiple labels are named "x"
Line 2, characters 2-3:
2 |   x: unit;
      ^
  First definition was here
Line 3, characters 2-3:
3 |   x: unit;
      ^
  Duplicate definition here
Line 4, characters 2-3:
4 |   x: unit;
      ^
  Duplicate definition here
|}]
