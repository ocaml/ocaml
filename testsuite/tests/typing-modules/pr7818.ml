(* TEST
   * expect
*)

(* cannot_alias.ml *)
module Termsig = struct
  module Term0 = struct
    module type S = sig
      module Id : sig end
    end
  end
  module Term = struct
    module type S = sig
      module Term0 : Term0.S
      module T = Term0
    end
  end
end;;
[%%expect{|
module Termsig :
  sig
    module Term0 : sig module type S = sig module Id : sig end end end
    module Term :
      sig module type S = sig module Term0 : Term0.S module T = Term0 end end
  end
|}]

module Make1 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    let u = 1
  end
end;;
[%%expect{|
module Make1 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig end end
          end)
    -> sig module T : sig module Id : sig end val u : int end end
|}]

module Make2 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    module Id2 = Id
    let u = 1
  end
end;;
[%%expect{|
module Make2 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig end end
          end)
    ->
    sig
      module T : sig module Id : sig end module Id2 = Id val u : int end
    end
|}]

module Make3 (T' : Termsig.Term.S) = struct
  module T = struct
    include T'.T
    module Id2 = Id
    let u = 1
    let u = 1
  end
end;;
[%%expect{|
module Make3 :
  functor
    (T' : sig
            module Term0 : Termsig.Term0.S
            module T : sig module Id : sig end end
          end)
    ->
    sig
      module T : sig module Id : sig end module Id2 = Id val u : int end
    end
|}]

(* cannot_alias2.ml *)
module type S = sig
  module Term0 : sig module Id : sig end end
  module T = Term0
end;;

module Make1 (T' : S)  = struct
  module Id = T'.T.Id
  module Id2 = Id
end;;
[%%expect{|
module type S =
  sig module Term0 : sig module Id : sig end end module T = Term0 end
module Make1 :
  functor
    (T' : sig
            module Term0 : sig module Id : sig end end
            module T : sig module Id : sig end end
          end)
    -> sig module Id : sig end module Id2 = Id end
|}]

module Make2 (T' : S) : sig module Id : sig end module Id2 = Id end
                        with module Id := T'.Term0.Id  = struct
  module Id = T'.T.Id
  module Id2 = Id
end;;
[%%expect{|
Lines 2-5, characters 57-3:
2 | .........................................................struct
3 |   module Id = T'.T.Id
4 |   module Id2 = Id
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module Id : sig end module Id2 = Id end
       is not included in
         sig module Id2 = T'.Term0.Id end
       In module Id2:
       Module T'.Term0.Id cannot be aliased
|}]

module Make3 (T' : S) = struct
  module T = struct
    module Id = T'.T.Id
    module Id2 = Id
    let u = 1
    let u = 1
  end
end;;
[%%expect{|
module Make3 :
  functor
    (T' : sig
            module Term0 : sig module Id : sig end end
            module T : sig module Id : sig end end
          end)
    ->
    sig
      module T : sig module Id : sig end module Id2 = Id val u : int end
    end
|}]

(* unsoundness if Make1 returned an Id.x field *)
module M = Make1 (struct module Term0 =
  struct module Id = struct let x = "a" end end module T = Term0 end);;
M.Id.x;;
[%%expect{|
module M : sig module Id : sig end module Id2 = Id end
Line 3, characters 0-6:
3 | M.Id.x;;
    ^^^^^^
Error: Unbound value M.Id.x
|}]


(* cannot_alias3.ml *)
module MkT(X : sig end) = struct type t end
module type S = sig
  module Term0 : sig module Id : sig end end
  module T = Term0
  type t = MkT(T).t
end;;

module Make1 (T' : S)  = struct
  module Id = T'.T.Id
  module Id2 = Id
  type t = T'.t
end;;

module IS = struct
  module Term0 = struct module Id = struct let x = "a" end end
  module T = Term0
  type t = MkT(T).t
end;;

module M = Make1(IS);;
[%%expect{|
module MkT : functor (X : sig end) -> sig type t end
module type S =
  sig
    module Term0 : sig module Id : sig end end
    module T = Term0
    type t = MkT(T).t
  end
module Make1 :
  functor
    (T' : sig
            module Term0 : sig module Id : sig end end
            module T : sig module Id : sig end end
            type t = MkT(T).t
          end)
    -> sig module Id : sig end module Id2 = Id type t = T'.t end
module IS :
  sig
    module Term0 : sig module Id : sig val x : string end end
    module T = Term0
    type t = MkT(T).t
  end
module M : sig module Id : sig end module Id2 = Id type t = IS.t end
|}]


(* cannot_alias4.ml *)
(* Can be used to break module abstraction *)
(* Still sound ? *)
(* It seems to only work if Term0 and Term contain identical types *)
(* It may also be possible to do the same thing using
   Mtype.nondep_supertype anyway *)
type (_,_) eq = Eq : ('a,'a) eq
module MkT(X : Set.OrderedType) = Set.Make(X)
module type S = sig
  module Term0 : Set.OrderedType with type t = int
  module T = Term0
  type t = E of (MkT(T).t,MkT(T).t) eq
  type u = t = E of (MkT(Term0).t,MkT(T).t) eq
end;;
module F(X:S) = X;;
module rec M : S = M;;
module M' = F(M);;
module type S' = module type of M';;
module Asc = struct type t = int let compare x y = x - y end;;
module Desc = struct type t = int let compare x y = y - x end;;
module rec M1 : S' with module Term0 := Asc and module T := Desc = M1;;
(* And now we have a witness of MkT(Asc).t = MkT(Desc).t ... *)
let (E eq : M1.u) = (E Eq : M1.t);;
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq
module MkT :
  functor (X : Set.OrderedType) ->
    sig
      type elt = X.t
      type t = Set.Make(X).t
      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
    end
module type S =
  sig
    module Term0 : sig type t = int val compare : t -> t -> int end
    module T = Term0
    type t = E of (MkT(T).t, MkT(T).t) eq
    type u = t = E of (MkT(Term0).t, MkT(T).t) eq
  end
module F :
  functor
    (X : sig
           module Term0 : sig type t = int val compare : t -> t -> int end
           module T : sig type t = int val compare : t -> t -> int end
           type t = E of (MkT(T).t, MkT(T).t) eq
           type u = t = E of (MkT(Term0).t, MkT(T).t) eq
         end)
    ->
    sig
      module Term0 : sig type t = int val compare : t -> t -> int end
      module T : sig type t = int val compare : t -> t -> int end
      type t = X.t = E of (MkT(T).t, MkT(T).t) eq
      type u = t = E of (MkT(Term0).t, MkT(T).t) eq
    end
module rec M : S
module M' :
  sig
    module Term0 : sig type t = int val compare : t -> t -> int end
    module T : sig type t = int val compare : t -> t -> int end
    type t = M.t = E of (MkT(T).t, MkT(T).t) eq
    type u = t = E of (MkT(Term0).t, MkT(T).t) eq
  end
module type S' =
  sig
    module Term0 : sig type t = int val compare : t -> t -> int end
    module T : sig type t = int val compare : t -> t -> int end
    type t = M.t = E of (MkT(T).t, MkT(T).t) eq
    type u = t = E of (MkT(Term0).t, MkT(T).t) eq
  end
module Asc : sig type t = int val compare : int -> int -> int end
module Desc : sig type t = int val compare : int -> int -> int end
Line 15, characters 0-69:
15 | module rec M1 : S' with module Term0 := Asc and module T := Desc = M1;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type M.t
       Constructors do not match:
         E of (MkT(M.T).t, MkT(M.T).t) eq
       is not the same as:
         E of (MkT(Desc).t, MkT(Desc).t) eq
       The type (MkT(M.T).t, MkT(M.T).t) eq is not equal to the type
         (MkT(Desc).t, MkT(Desc).t) eq
       Type MkT(M.T).t = Set.Make(M.Term0).t is not equal to type
         MkT(Desc).t = Set.Make(Desc).t
|}]
