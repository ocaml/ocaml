(* TEST
  * expect
*)



module Root = struct
  type u
  and t = private < .. >
end

module Trunk = struct
  include Root
  type t = A
  type u
end

module M: sig
  module type s = module type of Trunk
end = struct
  module type s = sig
    type t = A
    type u
  end
end
[%%expect {|
module Root : sig type u and t = private < .. > end
module Trunk : sig type t = A type u end
module M : sig module type s = sig type t = A type u end end
|}]
