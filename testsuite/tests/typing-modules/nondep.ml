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
Line 1, characters 11-35:
1 | module M = F(struct type t = T end);;
               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This functor has type
       functor (X : sig type t end) -> sig val f : X.t -> unit end
       The parameter cannot be eliminated in the result type.
       Please bind the argument to a module identifier.
|}]
