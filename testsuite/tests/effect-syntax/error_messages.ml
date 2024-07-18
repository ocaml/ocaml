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
Error: The value "k" has type "(%eff, unit) continuation"
       but an expression was expected of type "unit"
|}, Principal{|
Line 3, characters 21-22:
3 |   | effect A _, k -> k
                         ^
Error: The value "k" has type "(int, unit) continuation"
       but an expression was expected of type "unit"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

let () = match () with
  | () -> ()
  | effect A, [k] -> ()
[%%expect {|
Line 3, characters 14-17:
3 |   | effect A, [k] -> ()
                  ^^^
Error: Invalid continuation pattern: only variables and _ are allowed .
|}]

let () = match [] with
  | _ -> ()
  | [effect A, k] -> ()
[%%expect {|
Line 3, characters 5-16:
3 |   | [effect A, k] -> ()
         ^^^^^^^^^^^
Error: Effect patterns must be at the top level of a match case.
|}]
