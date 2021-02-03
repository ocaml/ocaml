(* TEST
   * expect
*)

type i = <m : 'c. 'c -> 'c >
type ('a, 'b) j = <m : 'c. 'a -> 'b >
type _ t = A : i t;;
[%%expect{|
type i = < m : 'c. 'c -> 'c >
type ('a, 'b) j = < m : 'a -> 'b >
type _ t = A : i t
|}]

let f (type a b) (y : (a, b) j t) : a -> b =
  let A = y in fun x -> x;;
[%%expect{|
Line 2, characters 6-7:
2 |   let A = y in fun x -> x;;
          ^
Error: This pattern matches values of type i t
       but a pattern was expected which matches values of type (a, b) j t
       Type i = < m : 'c. 'c -> 'c > is not compatible with type
         (a, b) j = < m : a -> b >
       The method m has type 'c. 'c -> 'c, but the expected method type was
       a -> b
       The universal variable 'c would escape its scope
|}]
