(* TEST
   * expect
*)

module F = functor (Y: sig end) -> struct
  module type T = sig module X = Y end
end
[%%expect{|
module F :
  functor (Y : sig end) -> sig module type T = sig module X : sig end end end
|}]
(* Notice that the signature of X above
   has been inlined from (X = Y) to (X : sig end). *)

module M = F(struct end)
[%%expect{|
module M : sig module type T = sig module X : sig end end end
|}]

module type T
module type T1 = functor (Y:T) -> sig module X1 = Y module X2 = Y end
module type T2 = functor (Y:T) -> sig module X1 : T module X2 = X1 end
module TestSub(F:T1) = (F:T2)
[%%expect{|
module type T
module type T1 = functor (Y : T) -> sig module X1 : T module X2 : T end
module type T2 = functor (Y : T) -> sig module X1 : T module X2 = X1 end
Line 4, characters 24-25:
4 | module TestSub(F:T1) = (F:T2)
                            ^
Error: Signature mismatch:
       Modules do not match:
         functor (Y : T) -> sig module X1 : T module X2 : T end
       is not included in
         T2
       Modules do not match:
         sig module X1 : T module X2 : T end
       is not included in
         sig module X1 : T module X2 = X1 end
       In module X2:
       Modules do not match: T is not included in (module X1)
|}]
