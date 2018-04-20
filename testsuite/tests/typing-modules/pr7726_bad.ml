(* TEST
   * expect
*)

module type T = sig type t end;;
[%%expect{|
module type T = sig type t end
|}]

module Fix(F:(T -> T)) = struct
  module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
end;;
[%%expect{|
Line _, characters 21-47:
    module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation Fixed.t is cyclic
|}]

(*
module T = Fix(functor (X:sig type t end) -> struct type t = X.t option end)

module T = Fix(functor (X:sig type t end) -> struct type t = X.t end) *)

module M = struct
  module F (X : T) : T = X
  module rec Fixed : sig type t = F(Fixed).t end = Fixed
end;;

module type S = module type of M;;

module Id (X : T) = X;;

module type Bad = S with module F = Id;;
