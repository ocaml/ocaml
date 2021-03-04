(* TEST
   * expect
*)


(* If a module is used as a module type it should trigger the hint. *)
module Equal = struct end
module Foo = functor (E : Equal) -> struct end;;
[%%expect{|
module Equal : sig end
Line 2, characters 26-31:
2 | module Foo = functor (E : Equal) -> struct end;;
                              ^^^^^
Error: Unbound module type Equal
Hint: There is a module named Equal, but modules are not module types
|}]

(* If there is a typo in the module type name it should trigger the
   spellcheck.
*)
module type Equals = sig end
module Foo = functor (E : EqualF) -> struct end;;
[%%expect{|
module type Equals = sig end
Line 2, characters 26-32:
2 | module Foo = functor (E : EqualF) -> struct end;;
                              ^^^^^^
Error: Unbound module type EqualF
Hint: Did you mean Equals?
|}]

(* If a module is used as a module type it should trigger the hint
   (even it is a typo). *)
module type Equal = sig end
module EqualF = struct end
module Foo = functor (E : EqualF) -> struct end;;
[%%expect{|
module type Equal = sig end
module EqualF : sig end
Line 3, characters 26-32:
3 | module Foo = functor (E : EqualF) -> struct end;;
                              ^^^^^^
Error: Unbound module type EqualF
Hint: There is a module named EqualF, but modules are not module types
|}]

(* If a module type is used as a module it should trigger the hint. *)
module type S = sig type t val show: t -> string end
let f (x: S.t ) = ();;
[%%expect{|
module type S = sig type t val show : t -> string end
Line 2, characters 10-13:
2 | let f (x: S.t ) = ();;
              ^^^
Error: Unbound module S
Hint: There is a module type named S, but module types are not modules
|}]

(* If a class type is used as a class it should trigger the hint. *)
class type ct = object method m: int end
class c = object inherit ct end
[%%expect{|
class type ct = object method m : int end
Line 2, characters 25-27:
2 | class c = object inherit ct end
                             ^^
Error: Unbound class ct
Hint: There is a class type named ct, but classes are not class types
|}]
