(* TEST
   * expect
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
Error: The function 'print_lines' has type string list -> unit
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
Error: The function 't.f' has type int -> unit
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
Error: The function 't#f' has type int -> unit
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
Error: The function 'a' has type 'a -> unit
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
