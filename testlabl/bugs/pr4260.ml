module type S =
sig
  module T : sig end
  type t
end
module rec U : S with module T = U
  = struct end;;
