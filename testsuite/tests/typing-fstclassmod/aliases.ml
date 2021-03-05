(* TEST
   * expect
*)

module M = struct end

module type S = sig
  module Alias = M

  type t
end

module type T = S with type t = int

let h x = (x : (module S with type t = int) :> (module T))
;;
[%%expect {|
module M : sig end
module type S = sig module Alias = M type t end
module type T = sig module Alias = M type t = int end
val h : (module S with type t = int) -> (module T) = <fun>
|}]
