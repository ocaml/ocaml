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
Line 11, characters 10-58:
11 | let h x = (x : (module S with type t = int) :> (module T))
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type (module S with type t = int) is not a subtype of (module T)
|}]
