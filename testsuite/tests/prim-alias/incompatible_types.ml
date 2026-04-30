(* TEST
   expect;
*)

external id : 'a. 'a -> 'a = "%identity"

(* We reject impossible types which are explicitly distinguishable. *)
type u = A
module type S = sig external x : u = id end

[%%expect{|
external id : 'a -> 'a = "%identity"
type u = A
Line 5, characters 33-34:
5 | module type S = sig external x : u = id end
                                     ^
Error: The type of this alias does not match that of the aliased primitive.
       Type "u" is not compatible with type "'a -> 'a"
|}]

(* We reject impossible types from the initial environment. *)
module type S = sig external x : int = id end

[%%expect{|
Line 1, characters 33-36:
1 | module type S = sig external x : int = id end
                                     ^^^
Error: The type of this alias does not match that of the aliased primitive.
       Type "int" is not compatible with type "'a -> 'a"
|}]
