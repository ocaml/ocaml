module Foo : sig type t [@@immediate] end = struct
  type t = string
end
