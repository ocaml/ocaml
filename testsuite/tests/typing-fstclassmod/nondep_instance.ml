(* TEST
 expect;
*)

module type Vector_space = sig
  type t
  type scalar
  val scale : scalar -> t -> t
end;;
[%%expect{|
module type Vector_space =
  sig type t type scalar val scale : scalar -> t -> t end
|}];;

module type Scalar = sig
  type t
  include Vector_space with type t := t
                        and type scalar = t
end;;
[%%expect{|
module type Scalar =
  sig type t type scalar = t val scale : scalar -> t -> t end
|}];;

module type Linear_map = sig
  type ('a, 'b) t
  val scale :
    (module Vector_space with type t = 'a and type scalar = 'l) ->
    'l -> ('a, 'a) t
end;;
[%%expect{|
module type Linear_map =
  sig
    type ('a, 'b) t
    val scale :
      (module Vector_space with type scalar = 'l and type t = 'a) ->
      'l -> ('a, 'a) t
  end
|}];;

module Primitive(Linear_map : Linear_map) = struct
  let f (type s) (s : (module Scalar with type t = s)) x =
    Linear_map.scale s x
end;;
[%%expect{|
Line 3, characters 21-22:
3 |     Linear_map.scale s x
                         ^
Error: The value "s" has type "(module Scalar with type t = s)"
       but an expression was expected of type
         "(module Vector_space with type scalar = 'a and type t = 'b)"
       The type "scalar" depends on internal types in the first module type.
|}];;

module type A = sig type a type b = a end
module type B = sig type a type b end
let g (type t s) (_:(module B with type a = t and type b = s)) = ()
[%%expect {|
module type A = sig type a type b = a end
module type B = sig type a type b end
val g : (module B with type a = 't and type b = 's) -> unit = <fun>
|}]

let f (x: (module A with type a = int)) =
  (x: (module B with type a = int and type b = int))
[%%expect {|
Line 2, characters 3-4:
2 |   (x: (module B with type a = int and type b = int))
       ^
Error: The value "x" has type "(module A with type a = int)"
       but an expression was expected of type
         "(module B with type a = int and type b = int)"
       The type "b" depends on internal types in the first module type.
|}]
