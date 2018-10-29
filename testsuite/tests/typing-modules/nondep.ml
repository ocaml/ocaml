(* TEST
   * expect
*)

module F(X : sig type t end) = struct
  let f (_ : X.t) = ()
end;;
[%%expect{|
module F : functor (X : sig type t end) -> sig val f : X.t -> unit end
|}]

module M = F(struct type t = T end);;
[%%expect{|
Uncaught exception: Ctype.Nondep_cannot_erase(_)

|}]
