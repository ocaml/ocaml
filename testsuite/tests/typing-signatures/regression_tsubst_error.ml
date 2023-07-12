(* TEST
   expect;
*)
type t = bool
module type Subst = sig
  type t2 := t
  type _ s = C : 'a -> (t * t2 * 'a) s
end
[%%expect{|
type t = bool
module type Subst = sig type _ s = C : 'a -> (t * t * 'a) s end
|}]
