(*TEST
  expect;
*)
(* Error messages for non syntactic type mismatches *)
module type D = sig type d end
module type Empty = sig end
module type A = sig type a = char type b type c = float  end
module type B = sig type a type b = int type c type err end
module type C = sig type a type 'a b type c end
let f (x: (module A with type b = int))=
  (x:(module B with type a = char and type c = float and type err = string))
[%%expect {|
module type D = sig type d end
module type Empty = sig end
module type A = sig type a = char type b type c = float end
module type B = sig type a type b = int type c type err end
module type C = sig type a type 'a b type c end
Line 7, characters 3-4:
7 |   (x:(module B with type a = char and type c = float and type err = string))
       ^
Error: The value "x" has type "(module A with type b = int)"
       but an expression was expected of type
         "(module B with type a = char and type c = float and type err =
          string)"
       There is no type "err" in the first module type.
|}]


let f (x: (module A with type b = int))=
  (x:(module C with type a = char and type c = float))
[%%expect {|
Line 2, characters 3-4:
2 |   (x:(module C with type a = char and type c = float))
       ^
Error: The value "x" has type "(module A with type b = int)"
       but an expression was expected of type
         "(module C with type a = char and type c = float)"
       The constraint on "b" in the first module type is not compatible
       with the declaration of type 'a b in the second module type.
|}]

module type U = sig
  include sig type t end
  type u = t option
end

module type X = sig type t end
module type Y = sig type t = <m: t * t> end
let f (x: (module X with type t = < m : 'a * 'a > as 'a)) = (x : (module Y));;
[%%expect {|
module type U = sig type t type u = t option end
module type X = sig type t end
module type Y = sig type t = < m : t * t > end
val f : (module X with type t = < m : 'a * 'a > as 'a) -> (module Y) = <fun>
|}]

let f (x: (module A with type b = int))= (x :> (module D))
[%%expect {|
Line 1, characters 41-58:
1 | let f (x: (module A with type b = int))= (x :> (module D))
                                             ^^^^^^^^^^^^^^^^^
Error: Type "(module A with type b = int)" is not a subtype of "(module D)"
       Modules do not match:
         sig type a = char type b = int type c = float end
       is not included in
         D
       The type "d" is required but not provided
|}]


let f (x: (module A with type b = int))= (x :> (module Empty))
[%%expect {|
val f : (module A with type b = int) -> (module Empty) = <fun>
|}]

module type A = sig type a type b = a type c =int end
module type B = sig type b = int type c = float end

let f (x: (module A with type a = int)) = (x :> (module B))
[%%expect {|
module type A = sig type a type b = a type c = int end
module type B = sig type b = int type c = float end
Line 4, characters 42-59:
4 | let f (x: (module A with type a = int)) = (x :> (module B))
                                              ^^^^^^^^^^^^^^^^^
Error: Type "(module A with type a = int)" is not a subtype of "(module B)"
       Modules do not match:
         sig type a = int type b = a type c = int end
       is not included in
         B
       Type declarations do not match:
         type c = int
       is not included in
         type c = float
       The type "int" is not equal to the type "float"
|}]
