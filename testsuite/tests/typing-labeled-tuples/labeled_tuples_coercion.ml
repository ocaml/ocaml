(* TEST
   expect;
*)

(* Testing coercion across labels *)

let coerce_tuple x = ((x : int * string :> x:int * y:string) :> foo:int * string)
[%%expect{|
val coerce_tuple : int * string -> foo:int * string = <fun>
|}]

let coerce_tuple x = (x : int * string :> x:string * y:int)
[%%expect{|
Line 1, characters 21-59:
1 | let coerce_tuple x = (x : int * string :> x:string * y:int)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int * string" is not a subtype of "x:string * y:int"
       Type "int" is not a subtype of "string"
|}]

let no_implicit_coercion (x : int * string) = x
let _ = no_implicit_coercion (1, ~y:"hello")
[%%expect{|
val no_implicit_coercion : int * string -> int * string = <fun>
Line 2, characters 29-44:
2 | let _ = no_implicit_coercion (1, ~y:"hello")
                                 ^^^^^^^^^^^^^^^
Error: This expression has type "int * y:'a"
       but an expression was expected of type "int * string"
       The first tuple element is labeled "y",
       but an unlabeled element was expected
|}]

(* Label coercion respects private types *)

type private_labeled_tuple = private x:int * y:int
type private_unlabeled_tuple = private int * int

let coerce_across_private_types (x : private_labeled_tuple) = (x :> private_unlabeled_tuple)
[%%expect{|
type private_labeled_tuple = private x:int * y:int
type private_unlabeled_tuple = private int * int
Line 4, characters 62-92:
4 | let coerce_across_private_types (x : private_labeled_tuple) = (x :> private_unlabeled_tuple)
                                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "private_labeled_tuple" is not a subtype of "private_unlabeled_tuple"
|}]

let coerce_from_private_to_public (x : private_unlabeled_tuple) = (x :> x:int * y:int)
[%%expect{|
val coerce_from_private_to_public : private_unlabeled_tuple -> x:int * y:int =
  <fun>
|}]

let coerce_from_public_to_private (x : x:int * y:int) = (x :> private_unlabeled_tuple)
[%%expect{|
Line 1, characters 56-86:
1 | let coerce_from_public_to_private (x : x:int * y:int) = (x :> private_unlabeled_tuple)
                                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "x:int * y:int" is not a subtype of "private_unlabeled_tuple"
|}]

(* Label coercion works in any variance direction *)

(* Motivating example *)

let find_child (rels : (parent:string * child:string) list) ~child =
  List.assoc child (rels :> (string * string) list)
;;

let lift_result (x : string -> foo:int * string) = (x :> string -> int * bar:string)

[%%expect{|
val find_child :
  (parent:string * child:string) list -> child:string -> string = <fun>
val lift_result : (string -> foo:int * string) -> string -> int * bar:string =
  <fun>
|}]

(* Label coercion doesn't work across invariant types *)
type !'a injective
type 'a noninjective

let coerce_injective (x : (int * string) injective) = (x :> (foo:int * string) injective)

[%%expect{|
type !'a injective
type 'a noninjective
Line 4, characters 54-89:
4 | let coerce_injective (x : (int * string) injective) = (x :> (foo:int * string) injective)
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(int * string) injective" is not a subtype of
         "(foo:int * string) injective"
       A label "foo" was expected
|}]

let coerce_noninjective (x : (int * string) noninjective) = (x :> (foo:int * string) noninjective)

[%%expect{|
Line 1, characters 60-98:
1 | let coerce_noninjective (x : (int * string) noninjective) = (x :> (foo:int * string) noninjective)
                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(int * string) noninjective" is not a subtype of
         "(foo:int * string) noninjective"
       A label "foo" was expected
|}]
