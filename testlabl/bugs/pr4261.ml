module type S =
sig
  type t
end

module type T = 
sig
  module D : S
  type t = D.t
end

module rec U' : S  with type t = U'.t = U
and U : T with module D = U' = U

