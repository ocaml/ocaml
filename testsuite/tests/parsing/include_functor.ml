(* TEST
 expect;
*)

(* Test that include functor is printed with a keyword and not an attribute *)
module type S1 = sig
  type t
  val x : t
end

module type F_res = sig
  type t
  val z : t
end

module type F = functor (X : S1) -> sig val z : X.t end

module type S2 = sig
  type t
  val x : t
  include F_res with type t := t
end

module G (X : S1) = struct
  let z = X.x
end

module M : S2 = struct
  type t = int
  let x = 3
  include functor G
end;;
[%%expect {|
module type S1 = sig type t val x : t end
module type F_res = sig type t val z : t end
module type F = (X : S1) -> sig val z : X.t end
module type S2 = sig type t val x : t val z : t end
module G : (X : S1) -> sig val z : X.t end
module M : S2
|}];;
