(* TEST
  * expect
*)

(* Test that anonymous argument that appears in the resulting module type of a
   functor (due to strengthening) are properly named by the typehecker
   pretty-printer
*)

(* We use Arg_n as argument names, thus the following definition are here to
   tests that the naming function avoid names pre-existing in the printing environment.
*)
module Arg_0 = struct end
module Arg_2 = struct end


module type Empty = sig end
module type T = sig type t end;;
[%%expect {|
module Arg_0 : sig  end
module Arg_2 : sig  end
module type Empty = sig  end
module type T = sig type t end
|}]



module Id(X: Empty -> T) = X;;
[%%expect {|
module Id :
  functor (X : Empty -> T) (Arg_1 : Empty) -> sig type t = X(Arg_1).t end
|}]


module type Ty
module type Endo = Ty -> Ty
module Compose(F:Endo->Endo)(G:Endo->Endo)(X:Endo) = F(G(X))
[%%expect {|
module type Ty
module type Endo = Ty -> Ty
module Compose :
  functor (F : Endo -> Endo) (G : Endo -> Endo) (X : Endo) -> Ty -> Ty
|}]


module type Mt = sig module type t end
module Succ(A:Mt) = struct module type t = A.t -> A.t end
module Zero = struct module type t = T end
module Five = Succ(Succ(Succ(Succ(Succ(Zero)))));;
[%%expect {|
module type Mt = sig module type t end
module Succ : functor (A : Mt) -> sig module type t = A.t -> A.t end
module Zero : sig module type t = T end
module Five :
  sig
    module type t =
      Succ(Succ(Succ(Succ(Zero)))).t -> Succ(Succ(Succ(Succ(Zero)))).t
  end
|}]


module Compose_5(Arg_4:Five.t->Five.t)(Arg_6:Five.t->Five.t)(Arg_8:Five.t) = Arg_4(Arg_6(Arg_8));;
[%%expect {|
module Compose_5 :
  functor (Arg_4 : Five.t -> Five.t) (Arg_6 : Five.t -> Five.t)
    (Arg_8 : Five.t) (Arg_9 : Succ(Succ(Succ(Succ(Zero)))).t)
    (Arg_7 : Succ(Succ(Succ(Zero))).t) (Arg_5 : Succ(Succ(Zero)).t)
    (Arg_3 : Succ(Zero).t) (Arg_1 : Zero.t) ->
    sig type t = Arg_4(Arg_6(Arg_8))(Arg_9)(Arg_7)(Arg_5)(Arg_3)(Arg_1).t end
|}]


module type Ta = sig
  module Arg_1: sig end
  type u
  module Arg_3: sig end
  type t
  module Arg_5: sig end
end;;
[%%expect {|
module type Ta =
  sig
    module Arg_1 : sig  end
    type u
    module Arg_3 : sig  end
    type t
    module Arg_5 : sig  end
  end
|}]


module F(Arg_4: Ta -> Ta) = Arg_4
[%%expect {|
module F :
  functor (Arg_4 : Ta -> Ta) (Arg_5 : Ta) ->
    sig
      module Arg_1 : sig  end
      type u = Arg_4(Arg_5).u
      module Arg_3 : sig  end
      type t = Arg_4(Arg_5).t
      module Arg_5 : sig  end
    end
|}]

module type With_subfunctor = sig
  module F: functor(Arg_1:Empty) -> Empty ->  Mt
end
[%%expect {|
module type With_subfunctor =
  sig module F : functor (Arg_1 : Empty) -> Empty -> Mt end
|}]


module Id(X: Empty -> With_subfunctor) = X
[%%expect {|
module Id :
  functor (X : Empty -> With_subfunctor) (Arg_4 : Empty) ->
    sig
      module F :
        functor (Arg_1 : Empty) (Arg_3 : Empty) ->
          sig module type t = X(Arg_4).F(Arg_1)(Arg_3).t end
    end
|}]
