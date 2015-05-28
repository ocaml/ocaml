(* Can't ascribe to an immediate type signature with a non-immediate type *)
module Foo : sig type t [@@immediate] end = struct
  type t = string
end
