module type S = sig type t end
module F (A : S) = struct
  type t2 = A.t
  module A = A
end

module A = struct type t = int end

module type S3 = sig
  module M : S
end

module type S2 = S with type t = F(A).t2

module type S4 = S3 with module M = F(A).A
