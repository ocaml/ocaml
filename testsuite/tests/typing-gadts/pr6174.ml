(* TEST
 expect;
*)

type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
let f : type a o. ((a -> o) -> o) t -> (a -> o) -> o =
 fun C k -> k (fun x -> x);;
[%%expect{|
type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
Line 3, characters 24-25:
3 |  fun C k -> k (fun x -> x);;
                            ^
Error: The value "x" has type "$'a" but an expression was expected of type
         "$'o" = "o"
|}];;
