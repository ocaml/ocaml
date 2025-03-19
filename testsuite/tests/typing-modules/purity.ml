(* TEST
  expect;
*)

module type T = sig module type S end

module type PureF = functor (T : T) => sig module M : T.S end

module type ImpureF = functor (T : T) -> sig module M : T.S end

module type PureFbis = functor [@pure] (T : T) -> sig module M : T.S end

[%%expect{|
module type T = sig module type S end
module type PureF = (T : T) => sig module M : T.S end
module type ImpureF = (T : T) -> sig module M : T.S end
module type PureFbis = (T : T) => sig module M : T.S end
|}]

module PuretoImpure (F : PureF) : sig
  module F : ImpureF
end = struct
  module F = F
end

[%%expect{|
module PuretoImpure : (F : PureF) -> sig module F : ImpureF end
|}]

module ImpureToPure_Fail (F : ImpureF) : sig
  module F : PureF
end = struct
  module F = F
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module F = F
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module F : (T : T) -> sig module M : T.S end end
       is not included in
         sig module F : PureF end
       In module "F":
       Modules do not match: (T : T) -> ... is not included in (T : T) -> ...
       The functor was expected to be generative at this position
|}]

module M (_ : T) = struct end
module Mimpure = functor (_ : T) -> struct end
module Mpure = functor (_ : T) => struct end
module M4 = functor (_ : T) (_ : T) -> struct end
module M5 = functor (_ : T) (_ : T) => struct end
module M6 = functor [@pure] (_ : T) (_ : T) -> struct end

[%%expect{|
module M : T -> sig end
module Mimpure : T -> sig end
module Mpure : T => sig end
module M4 : T => T -> sig end
module M5 : T => T => sig end
module M6 : T => T => sig end
|}]
