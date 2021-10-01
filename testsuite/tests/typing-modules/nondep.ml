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

module M (X : sig type 'a t constraint 'a = float end) = struct
  module type S = sig
    type t = float
    val foo : t X.t
  end
end

module N = M (struct type 'a t = int constraint 'a = float end)

[%%expect{|
module M :
  functor (X : sig type 'a t constraint 'a = float end) ->
    sig module type S = sig type t = float val foo : t X.t end end
module N : sig module type S = sig type t = float val foo : int end end
|}]

type 'a always_int = int
module F (X : sig type t end) = struct type s = X.t always_int end
module M = F (struct type t = T end)
[%%expect{|
type 'a always_int = int
module F : functor (X : sig type t end) -> sig type s = X.t always_int end
module M : sig type s = int end
|}]

module M = struct
  module F (X : sig type t end) = X
  module Not_ok = F (struct type t = private [< `A] end)
end
[%%expect{|
module M :
  sig
    module F : functor (X : sig type t end) -> sig type t = X.t end
    module Not_ok : sig type t end
  end
|}]
