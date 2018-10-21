(* TEST
    * expect
*)

module Variants = struct
  type bar = [ `Bar ]
  type foo = private [< `Foo | `Bar ]
end

open Variants

module M : sig
  type +'a t

  val foo : unit -> foo t
  val bar : unit -> bar t
end = struct
  type 'a t = 'a list

  let foo () = []
  let bar () = []
end

module type Foo = sig
  val x : foo M.t -> unit
end

let ffoo t (module F : Foo) =
  F.x t

module type Bar = sig
  val x : bar M.t -> unit
end

let fbar t (module B : Bar) =
  B.x t

let (foo : foo M.t) = M.foo ()
let (bar : bar M.t) = M.bar ()
[%%expect {|
module Variants :
  sig type bar = [ `Bar ] type foo = private [< `Bar | `Foo ] end
module M :
  sig
    type +'a t
    val foo : unit -> Variants.foo t
    val bar : unit -> Variants.bar t
  end
module type Foo = sig val x : Variants.foo M.t -> unit end
val ffoo : Variants.foo M.t -> (module Foo) -> unit = <fun>
module type Bar = sig val x : Variants.bar M.t -> unit end
val fbar : Variants.bar M.t -> (module Bar) -> unit = <fun>
val foo : Variants.foo M.t = <abstr>
val bar : Variants.bar M.t = <abstr>
|}]

let f1 = ffoo foo;;
[%%expect {|
val f1 : (module Foo) -> unit = <fun>
|}]

let f2 = ffoo bar;;
[%%expect {|
Line 1, characters 14-17:
1 | let f2 = ffoo bar;;
                  ^^^
Error: This expression has type Variants.bar M.t
       but an expression was expected of type Variants.foo M.t
       Type Variants.bar = [ `Bar ] is not compatible with type Variants.foo
       The first variant type does not allow tag(s) `Foo
|}]

let f3 = fbar foo;;
[%%expect {|
Line 1, characters 14-17:
1 | let f3 = fbar foo;;
                  ^^^
Error: This expression has type Variants.foo M.t
       but an expression was expected of type Variants.bar M.t
       Type Variants.foo is not compatible with type Variants.bar = [ `Bar ]
       The second variant type does not allow tag(s) `Foo
|}]
