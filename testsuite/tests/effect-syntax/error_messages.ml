(* TEST
  expect;
*)

type _ eff += A: 'a -> int eff
[%%expect {|
type _ eff += A : 'a -> int eff
|}]

let () = match () with
  | () -> ()
  | effect A k, k -> ()
[%%expect {|
Line 3, characters 13-14:
3 |   | effect A k, k -> ()
                 ^
Error: Variable "k" is bound several times in this matching
|}]

let () = match () with
  | () -> raise Not_found
  | effect A _, k -> k
[%%expect {|
Line 3, characters 21-22:
3 |   | effect A _, k -> k
                         ^
Error: This expression has type "(%eff, unit) continuation"
       but an expression was expected of type "unit"
|}, Principal{|
Line 3, characters 21-22:
3 |   | effect A _, k -> k
                         ^
Error: This expression has type "(int, unit) continuation"
       but an expression was expected of type "unit"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

let () = match () with
  | () -> ()
  | effect A _, [k] -> ()
[%%expect {|
Line 3, characters 4-19:
3 |   | effect A _, [k] -> ()
        ^^^^^^^^^^^^^^^
Error: Invalid continuation pattern: only variables and _ are allowed .
|}]

let () = match [] with
  | _ -> ()
  | [effect A _, k] -> ()
[%%expect {|
Line 3, characters 5-18:
3 |   | [effect A _, k] -> ()
         ^^^^^^^^^^^^^
Error: Effect patterns are not allowed in this position.
|}]

let () = match [] with
  | effect _, _ | exception Not_found -> ()
[%%expect {|
Lines 1-2, characters 9-43:
1 | .........match [] with
2 |   | effect _, _ | exception Not_found -> ()
Error: None of the patterns in this "match" expression match values.
|}]


let n = match 0 with
  | effect A _, _ -> 0
  | x -> 1
  | effect A _, _ -> 0
[%%expect {|
Line 4, characters 11-14:
4 |   | effect A _, _ -> 0
               ^^^
Warning 11 [redundant-case]: this match case is unused.

val n : int = 1
|}]
