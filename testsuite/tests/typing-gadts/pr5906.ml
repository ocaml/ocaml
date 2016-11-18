type _ constant =
  | Int: int -> int constant
  | Bool: bool -> bool constant

type (_, _, _) binop =
  | Eq: ('a, 'a, bool) binop
  | Leq: ('a, 'a, bool) binop
  | Add: (int, int, int) binop

let eval (type a) (type b) (type c) (bop:(a,b,c) binop) (x:a constant)
         (y:b constant) : c constant =
  match bop, x, y with
  | Eq, Bool x, Bool y -> Bool (if x then y else not y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | Leq, Bool x, Bool y -> Bool (x <= y)
  | Add, Int x, Int y -> Int (x + y)

let _ = eval Eq (Int 2) (Int 3)

[%%expect{|
type _ constant = Int : int -> int constant | Bool : bool -> bool constant
type (_, _, _) binop =
    Eq : ('a, 'a, bool) binop
  | Leq : ('a, 'a, bool) binop
  | Add : (int, int, int) binop
Line _, characters 2-195:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(Eq, Int _, _)
val eval : ('a, 'b, 'c) binop -> 'a constant -> 'b constant -> 'c constant =
  <fun>
Exception: Match_failure ("", 12, 2).
|}];;
