(* TEST
 expect;
*)

type t = Set.Make(String).t
[%%expect{|
type t = Set.Make(String).t
|} ]


(* Check the error messages of an ill-typed applicatived functor type. *)
module M = struct type t let equal = (=) end
[%%expect{|
module M : sig type t val equal : 'a -> 'a -> bool end
|} ]

type t = Set.Make(M).t
[%%expect{|
Line 1, characters 9-22:
1 | type t = Set.Make(M).t
             ^^^^^^^^^^^^^
Error: Modules do not match:
       sig type t = M.t val equal : 'a -> 'a -> bool end
     is not included in Set.OrderedType
     The value "compare" is required but not provided
     File "set.mli", line 55, characters 4-31: Expected declaration
|} ]


(* We would report the wrong error here if we didn't strengthen the
   type of the argument (type t wouldn't match). *)
module F(X : sig type t = M.t val equal : unit end)
  = struct type t end
[%%expect{|
module F : (X : sig type t = M.t val equal : unit end) -> sig type t end
|} ]

type t = F(M).t
[%%expect{|
Line 1, characters 9-15:
1 | type t = F(M).t
             ^^^^^^
Error: Modules do not match:
       sig type t = M.t val equal : 'a -> 'a -> bool end
     is not included in sig type t = M.t val equal : unit end
     Values do not match:
       val equal : 'a -> 'a -> bool
     is not included in
       val equal : unit
     The type "'a -> 'a -> bool" is not compatible with the type "unit"
|} ]


(* MPR#7611 *)
module Generative() = struct type t end
[%%expect{|
module Generative : () -> sig type t end
|}]

type t = Generative(M).t
[%%expect{|
Line 1, characters 9-24:
1 | type t = Generative(M).t
             ^^^^^^^^^^^^^^^
Error: The functor "Generative" is generative, it cannot be applied in type
       expressions
|}]



module F(X : sig module type S module F : S end) = struct
  type t = X.F(Parsing).t
end
[%%expect{|
Line 2, characters 11-25:
2 |   type t = X.F(Parsing).t
               ^^^^^^^^^^^^^^
Error: The module "X.F" is abstract, it cannot be applied
|}]
