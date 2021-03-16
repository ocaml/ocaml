(* TEST
   flags = " -w +A "
   * expect
*)

module Foo(Unused : sig end) = struct end;;
[%%expect {|
Line 1, characters 11-17:
1 | module Foo(Unused : sig end) = struct end;;
               ^^^^^^
Warning 60 [unused-module]: unused module Unused.
module Foo : functor (Unused : sig end) -> sig end
|}]

module type S = functor (Unused : sig end) -> sig end;;
[%%expect {|
Line 1, characters 25-31:
1 | module type S = functor (Unused : sig end) -> sig end;;
                             ^^^^^^
Warning 67 [unused-functor-parameter]: unused functor parameter Unused.
module type S = functor (Unused : sig end) -> sig end
|}]

module type S = sig
  module M (Unused : sig end) : sig end
end;;
[%%expect{|
Line 2, characters 12-18:
2 |   module M (Unused : sig end) : sig end
                ^^^^^^
Warning 67 [unused-functor-parameter]: unused functor parameter Unused.
module type S = sig module M : functor (Unused : sig end) -> sig end end
|}]
