(* TEST
 expect;
*)

let print_lines = List.iter print_endline

let () =
  print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
  print_endline "foo"
[%%expect{|
val print_lines : string list -> unit = <fun>
Lines 4-5, characters 2-15:
4 | ..print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
5 |   print_endline......
Error: The function "print_lines" has type string list -> unit
       It is applied to too many arguments
Line 4, characters 55-57:
4 |   print_lines (List.map string_of_int [ 1; 2; 3; 4; 5 ])
                                                           ^^
  Hint: Did you forget a ';'?
Line 5, characters 2-15:
5 |   print_endline "foo"
      ^^^^^^^^^^^^^
  This extra argument is not expected.
|}]

type t = { f : int -> unit }

let f (t : t) =
  t.f 1 2
[%%expect{|
type t = { f : int -> unit; }
Line 4, characters 2-9:
4 |   t.f 1 2
      ^^^^^^^
Error: The function "t.f" has type int -> unit
       It is applied to too many arguments
Line 4, characters 6-8:
4 |   t.f 1 2
          ^^
  Hint: Did you forget a ';'?
Line 4, characters 8-9:
4 |   t.f 1 2
            ^
  This extra argument is not expected.
|}]

let f (t : < f : int -> unit >) =
  t#f 1 2
[%%expect{|
Line 2, characters 2-9:
2 |   t#f 1 2
      ^^^^^^^
Error: The function "t#f" has type int -> unit
       It is applied to too many arguments
Line 2, characters 6-8:
2 |   t#f 1 2
          ^^
  Hint: Did you forget a ';'?
Line 2, characters 8-9:
2 |   t#f 1 2
            ^
  This extra argument is not expected.
|}]

let () =
  object
    val a = fun _ -> ()
    method b = a 1 2
  end
[%%expect{|
Line 4, characters 15-20:
4 |     method b = a 1 2
                   ^^^^^
Error: The function "a" has type 'a -> unit
       It is applied to too many arguments
Line 4, characters 17-19:
4 |     method b = a 1 2
                     ^^
  Hint: Did you forget a ';'?
Line 4, characters 19-20:
4 |     method b = a 1 2
                       ^
  This extra argument is not expected.
|}]

(* The result of [(+) 1 2] is not [unit], we don't expect the hint to insert a
   ';'. *)

let () =
  (+) 1 2 3
[%%expect{|
Line 2, characters 2-11:
2 |   (+) 1 2 3
      ^^^^^^^^^
Error: The function "(+)" has type int -> int -> int
       It is applied to too many arguments
Line 2, characters 10-11:
2 |   (+) 1 2 3
              ^
  This extra argument is not expected.
|}]

(* The arrow type might be hidden behind a constructor. *)

type t = int -> int -> unit
let f (x:t) = x 0 1 2
[%%expect{|
type t = int -> int -> unit
Line 2, characters 14-21:
2 | let f (x:t) = x 0 1 2
                  ^^^^^^^
Error: The function "x" has type int -> int -> unit
       It is applied to too many arguments
Line 2, characters 18-20:
2 | let f (x:t) = x 0 1 2
                      ^^
  Hint: Did you forget a ';'?
Line 2, characters 20-21:
2 | let f (x:t) = x 0 1 2
                        ^
  This extra argument is not expected.
|}]

type t = int -> unit
let f (x:int -> t) = x 0 1 2
[%%expect{|
type t = int -> unit
Line 2, characters 21-28:
2 | let f (x:int -> t) = x 0 1 2
                         ^^^^^^^
Error: The function "x" has type int -> t
       It is applied to too many arguments
Line 2, characters 25-27:
2 | let f (x:int -> t) = x 0 1 2
                             ^^
  Hint: Did you forget a ';'?
Line 2, characters 27-28:
2 | let f (x:int -> t) = x 0 1 2
                               ^
  This extra argument is not expected.
|}]
