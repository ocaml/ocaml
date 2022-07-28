(* TEST
   * expect
*)

module F = functor (Y: sig end) -> struct
  module type T = sig module X = Y end
end
[%%expect{|
module F :
  functor (Y : sig end) -> sig module type T = sig module X = Y end end
|}]
(* Inferring (module X = Y) above should not be allowed as Y is a functor argument.
   This causes a crash below. *)

module M = F(struct end)
[%%expect{|
>> Fatal error: nondep_supertype not included in original module type
Uncaught exception: Misc.Fatal_error

|}]
