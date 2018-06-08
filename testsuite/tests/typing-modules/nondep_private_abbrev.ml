(* TEST
   * expect
*)

module F(_ : sig end) : sig
  type t = private int
end = struct
  type t = int
end;;
[%%expect{|
module F : sig  end -> sig type t = private int end
|}]

module Direct = F(struct end);;
[%%expect{|
module Direct : sig type t = private int end
|}]

module G(X : sig end) : sig
  type t = F(X).t
end = F(X);;
[%%expect{|
module G : functor (X : sig  end) -> sig type t = F(X).t end
|}]

module Indirect = G(struct end);;
[%%expect{|
module Indirect : sig type t = private int end
|}]
