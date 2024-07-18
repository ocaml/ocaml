(* TEST
 expect;
*)

module type S = sig type u type t end;;
module type S' = sig type t = int type u = bool end;;

(* ok to convert between structurally equal signatures, and parameters
   are inferred *)
let f (x : (module S with type t = 'a and type u = 'b)) = (x : (module S'));;
let g x = (x : (module S with type t = 'a and type u = 'b) :> (module S'));;
[%%expect{|
module type S = sig type u type t end
module type S' = sig type t = int type u = bool end
val f : (module S with type t = int and type u = bool) -> (module S') = <fun>
val g : (module S with type t = int and type u = bool) -> (module S') = <fun>
|}];;

(* with subtyping it is also ok to forget some types *)
module type S2 = sig type u type t type w end;;
let g2 x = (x : (module S2 with type t = 'a and type u = 'b) :> (module S'));;
let h x = (x : (module S2 with type t = 'a) :> (module S with type t = 'a));;
let f2 (x : (module S2 with type t = 'a and type u = 'b)) =
  (x : (module S'));; (* fail *)
let k (x : (module S2 with type t = 'a)) =
  (x : (module S with type t = 'a));; (* fail *)
[%%expect{|
module type S2 = sig type u type t type w end
val g2 : (module S2 with type t = int and type u = bool) -> (module S') =
  <fun>
val h : (module S2 with type t = 'a) -> (module S with type t = 'a) = <fun>
Line 5, characters 3-4:
5 |   (x : (module S'));; (* fail *)
       ^
Error: The value "x" has type "(module S2 with type t = int and type u = bool)"
       but an expression was expected of type "(module S')"
       Modules do not match:
         S'
       is not included in
         sig type u = bool type t = int type w end
       The type "w" is required but not provided
|}];;

(* but you cannot forget values (no physical coercions) *)
module type S3 = sig type u type t val x : int end;;
let g3 x =
  (x : (module S3 with type t = 'a and type u = 'b) :> (module S'));; (* fail *)
[%%expect{|
module type S3 = sig type u type t val x : int end
Line 3, characters 2-67:
3 |   (x : (module S3 with type t = 'a and type u = 'b) :> (module S'));; (* fail *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module S3 with type t = int and type u = bool)"
       is not a subtype of "(module S')"
       The two first-class module types differ by their runtime size.
|}];;

(* but you cannot move values (no physical coercions) *)
module type S4 = sig val x : int  val mid:int  val y:int end
module type S5 = sig val x:int val y:int end
let g4 x =
  (x : (module S4) :> (module S5));; (* fail *)
[%%expect{|
module type S4 = sig val x : int val mid : int val y : int end
module type S5 = sig val x : int val y : int end
Line 4, characters 2-34:
4 |   (x : (module S4) :> (module S5));; (* fail *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module S4)" is not a subtype of "(module S5)"
       The two first-class module types do not share
       the same positions for runtime components.
       For example, the value "mid" occurs at the expected position of
       the value "y".
|}];;


let g5 x =
  (x : (module S5) :> (module S4));; (* fail *)
[%%expect{|
Line 2, characters 2-34:
2 |   (x : (module S5) :> (module S4));; (* fail *)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module S5)" is not a subtype of "(module S4)"
       Modules do not match: S5 is not included in S4
       The value "mid" is required but not provided
|}];;

module type Prim_Id = sig external id: 'a -> 'a = "%identity" end
module type Id = sig val id: 'a -> 'a end
module Named = struct end
module type Alias = sig module Alias = Named end
module type Nested = sig module Alias: sig end end
[%%expect {|
module type Prim_Id = sig external id : 'a -> 'a = "%identity" end
module type Id = sig val id : 'a -> 'a end
module Named : sig end
module type Alias = sig module Alias = Named end
module type Nested = sig module Alias : sig end end
|}]

let coerce_prim x = (x:(module Prim_Id):>(module Id))
[%%expect {|
Line 1, characters 20-53:
1 | let coerce_prim x = (x:(module Prim_Id):>(module Id))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module Prim_Id)" is not a subtype of "(module Id)"
       The two first-class module types differ by a coercion of
       the primitive "%identity" to a value.
|}]

let coerce_alias x = (x:(module Alias):>(module Nested))
[%%expect {|
Line 1, characters 21-56:
1 | let coerce_alias x = (x:(module Alias):>(module Nested))
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module Alias)" is not a subtype of "(module Nested)"
       The two first-class module types differ by a coercion of
       a module alias "Named" to a module.
|}]

module type Nested_coercion = sig
  module M: sig
    external identity: 'a -> 'a = "%identity"
  end
end


module type Nested_coercion_bis = sig
  module M: sig
    val identity: 'a -> 'a
  end
end

let coerce_prim' x = (x:(module Nested_coercion):>(module Nested_coercion_bis))

[%%expect{|
module type Nested_coercion =
  sig module M : sig external identity : 'a -> 'a = "%identity" end end
module type Nested_coercion_bis =
  sig module M : sig val identity : 'a -> 'a end end
Line 14, characters 21-79:
14 | let coerce_prim' x = (x:(module Nested_coercion):>(module Nested_coercion_bis))
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module Nested_coercion)" is not a subtype of
         "(module Nested_coercion_bis)"
       The two first-class module types differ by a coercion of
       the primitive "%identity" to a value, in module "M".
|}]
