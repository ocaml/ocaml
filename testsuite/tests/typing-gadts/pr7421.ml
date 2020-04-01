(* TEST
   * expect
*)

type (_, _) eq = Refl : ('a, 'a) eq;;
type empty = (int, unit) eq;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
type empty = (int, unit) eq
|}]
let f (x : ('a, empty Lazy.t) result) =
  match x with
  | Ok x -> x
  | Error (lazy _) -> .;;
[%%expect{|
Line 4, characters 4-18:
4 |   | Error (lazy _) -> .;;
        ^^^^^^^^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: Error lazy _
|}]
let f (x : ('a, empty Lazy.t) result) =
  match x with
  | Ok x -> x
  | Error (lazy Refl) -> .;;
[%%expect{|
Line 4, characters 16-20:
4 |   | Error (lazy Refl) -> .;;
                    ^^^^
Error: This pattern matches values of type (int, int) eq
       but a pattern was expected which matches values of type
         empty = (int, unit) eq
       Type int is not compatible with type unit
|}]
