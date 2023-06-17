(* TEST
 expect;
*)

module A = struct
  type r = int
  type s = string
  type t = unit
end;;

(* parens *)
type x = A.(r * s * t);;
[%%expect{|
module A : sig type r = int type s = string type t = unit end
type x = A.r * A.s * A.t
|}];;

(* variants *)
type a = A.[ `X of r | `Y of s | `Z of t ];;
[%%expect{|
type a = [ `X of A.r | `Y of A.s | `Z of A.t ]
|}];;

(* first class modules *)
module Comparable = struct
  module type S = sig
    type t
    val compare : t -> t -> int
  end
end

type t = Comparable.(module S)
[%%expect{|
module Comparable :
  sig module type S = sig type t val compare : t -> t -> int end end
type t = (module Comparable.S)
|}];;
