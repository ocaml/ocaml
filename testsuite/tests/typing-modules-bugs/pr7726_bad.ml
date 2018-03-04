module type T = sig type t end

module Fix(F:(T -> T)) = struct
  module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
end

module T = Fix(functor (X:sig type t end) -> struct type t = X.t option end)

module T = Fix(functor (X:sig type t end) -> struct type t = X.t end)
