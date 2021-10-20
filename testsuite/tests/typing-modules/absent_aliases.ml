(* TEST
   * expect
*)

(* Shadows aliases.ml, with absent modules as appropriate. *)

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' == N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' == N end);;
M2.N'.x;;

open M;;
N'.x;;
[%%expect{|
module M :
  sig
    module D : sig val y : int end
    module N : sig val x : int end
    module N' == N
  end
module M1 : sig module N : sig val x : int end module N' == N end
- : int = 1
module M2 : sig module N' : sig val x : int end end
- : int = 1
- : int = 1
|}];;

module M = struct
  module C = Char
  module C' = C
end;;
module M1
  : sig module C : sig val escaped : char -> string end module C' == C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' == C end);;
M2.C'.chr 66;;
[%%expect{|
module M : sig module C == Char module C' == C end
module M1 :
  sig module C : sig val escaped : char -> string end module C' == C end
- : string = "A"
module M2 : sig module C' : sig val chr : int -> char end end
- : char = 'B'
|}];;

module M = struct
  module A = struct
    module B = struct
      let run_me f = f ()
      let incr i = i + 1
    end
  end
  module A' = A
end;;
module M1 : sig
    module A : sig
      module B : sig
        val incr : int -> int
      end
    end
    module A' == A
  end =
M;; (* sound, but should probably fail *)
M1.A'.B.incr 0;;
module M2 : sig
    module A : sig
      module B : sig
        val incr : int -> int
      end
    end
    module A' == A
  end =
(M : sig
  module A : sig
    module B : sig
      val incr : int -> int
    end
  end
  module A' == A
end);;
M2.A'.B.incr 0;;
[%%expect{|
module M :
  sig
    module A :
      sig
        module B :
          sig val run_me : (unit -> 'a) -> 'a val incr : int -> int end
      end
    module A' == A
  end
module M1 :
  sig
    module A : sig module B : sig val incr : int -> int end end
    module A' == A
  end
- : int = 1
module M2 :
  sig
    module A : sig module B : sig val incr : int -> int end end
    module A' == A
  end
- : int = 1
|}];;

StdLabels.List.map;;
[%%expect{|
- : f:('a -> 'b) -> 'a list -> 'b list = <fun>
|}];;

module F(X:sig module C == Char end) = struct module C = X.C end;;
[%%expect{|
module F : functor (X : sig module C == Char end) -> sig module C == Char end
|}];;

module A1 = struct end
module A2 = struct end
module L1 = struct module X = A1 end
module L2 = struct module X = A2 end;;

module F (L : (module type of L1 [@remove_aliases])) = struct end;;

module F1 = F(L1);; (* ok *)
module F2 = F(L2);; (* should succeed too *)
[%%expect{|
module A1 : sig end
module A2 : sig end
module L1 : sig module X == A1 end
module L2 : sig module X == A2 end
module F : functor (L : sig module X : sig end end) -> sig end
module F1 : sig end
module F2 : sig end
|}];;
(* Counter example: why we need to be careful with PR#6307 *)
module Int = struct type t = int let compare = compare end
module SInt = Set.Make(Int)
type (_,_) eq = Eq : ('a,'a) eq
type wrap = W of (SInt.t, SInt.t) eq

module M = struct
  module I = Int
  type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
end;;
module type S = module type of M [@remove_aliases];; (* keep alias *)

module Int2 = struct type t = int let compare x y = compare y x end;;
module type S' = sig
  module I == Int2
  include S with module I := I
end;; (* fail *)
[%%expect{|
module Int : sig type t = int val compare : 'a -> 'a -> int end
module SInt :
  sig
    type elt = Int.t
    type t = Set.Make(Int).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
type (_, _) eq = Eq : ('a, 'a) eq
type wrap = W of (SInt.t, SInt.t) eq
module M :
  sig
    module I == Int
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
  end
module type S =
  sig
    module I == Int
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
  end
module Int2 : sig type t = int val compare : 'a -> 'a -> int end
Line 15, characters 10-30:
15 |   include S with module I := I
               ^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of I
       does not match its original definition in the constrained signature:
       Modules do not match: (module Int2) is not included in (module Int)
|}];;

(* (* if the above succeeded, one could break invariants *)
module rec M2 : S' = M2;; (* should succeed! (but this is bad) *)

let M2.W eq = W Eq;;

let s = List.fold_right SInt.add [1;2;3] SInt.empty;;
module SInt2 = Set.Make(Int2);;
let conv : type a b. (a,b) eq -> a -> b = fun Eq x -> x;;
let s' : SInt2.t = conv eq s;;
SInt2.elements s';;
SInt2.mem 2 s';; (* invariants are broken *)
*)

(* PR#6376 *)
module type Alias = sig module N : sig end module M == N end;;
module F (X : sig end) = struct type t end;;
module type A = Alias with module N := F(List);;
module rec Bad : A = Bad;;
[%%expect{|
module type Alias = sig module N : sig end module M == N end
module F : functor (X : sig end) -> sig type t end
Line 1:
Error: Module type declarations do not match:
         module type A = sig module M == F(List) end
       does not match
         module type A = sig module M == F(List) end
       At position module type A = <here>
       Module types do not match:
         sig module M == F(List) end
       is not equal to
         sig module M == F(List) end
       At position module type A = sig module M : <here> end
       Module F(List) cannot be aliased
|}];;

(* PR#6465 *)

module M = struct type t = A module B = struct type u = B end end;;
module P : sig type t = M.t = A module B == M.B end = M;;
module P : sig type t = M.t = A module B == M.B end = struct include M end;;
[%%expect{|
module M : sig type t = A module B : sig type u = B end end
module P : sig type t = M.t = A module B == M.B end
module P : sig type t = M.t = A module B == M.B end
|}];;

module type S = sig
  module M : sig module P : sig end end
  module Q == M
end;;
[%%expect{|
module type S = sig module M : sig module P : sig end end module Q == M end
|}];;
module type S = sig
  module M : sig module N : sig end module P : sig end end
  module Q : sig module N == M.N module P == M.P end
end;;
module R = struct
  module M = struct module N = struct end module P = struct end end
  module Q = M
end;;
module R' : S = R;;
[%%expect{|
module type S =
  sig
    module M : sig module N : sig end module P : sig end end
    module Q : sig module N == M.N module P == M.P end
  end
module R :
  sig
    module M : sig module N : sig end module P : sig end end
    module Q == M
  end
module R' : S
|}];;

(* PR#6578 *)

module M = struct let f x = x end
module rec R : sig module M : sig val f : 'a -> 'a end end =
  struct module M = M end;;
R.M.f 3;;
[%%expect{|
module M : sig val f : 'a -> 'a end
module rec R : sig module M : sig val f : 'a -> 'a end end
- : int = 3
|}];;
module rec R : sig module M == M end = struct module M = M end;;
R.M.f 3;;
[%%expect{|
module rec R : sig module M == M end
- : int = 3
|}];;

module M = struct type t end
module type S = sig module N == M val x : N.t end
module type T = S with module N := M;;
[%%expect{|
module M : sig type t end
module type S = sig module N == M val x : N.t end
module type T = sig val x : M.t end
|}];;


module X = struct module N = struct end end
module Y : sig
  module type S = sig module N == X.N end
end = struct
  module type S = module type of struct include X end
end;; (* Fails, module presence differs. *)
[%%expect{|
module X : sig module N : sig end end
Lines 4-6, characters 6-3:
4 | ......struct
5 |   module type S = module type of struct include X end
6 | end........................................
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig module N = X.N end end
       is not included in
         sig module type S = sig module N == X.N end end
       Module type declarations do not match:
         module type S = sig module N = X.N end
       does not match
         module type S = sig module N == X.N end
       At position module type S = <here>
       Illegal permutation of runtime components in a module type.
|}];;

module type S = sig
  module M : sig
    module A : sig end
    module B : sig end
  end
  module N == M.A
end

module Foo = struct
  module B = struct let x = 0 end
  module A = struct let x = "hello" end
end

module Bar : S with module M := Foo = struct module N = Foo.A end

let s : string = Bar.N.x
[%%expect {|
module type S =
  sig
    module M : sig module A : sig end module B : sig end end
    module N == M.A
  end
module Foo :
  sig module B : sig val x : int end module A : sig val x : string end end
module Bar : sig module N == Foo.A end
val s : string = "hello"
|}]


(* Coercion between present and absent aliases. *)
module type S_pres = sig module M : sig val x : int end module N = M end;;
module type S_abs = sig module M : sig val x : int end module N == M end;;
module M_abs = struct module M = struct let x = 3 end module N = M end;;
module M_pres : sig module M : sig val x : int end module N = M end = struct
  module M = struct let x = 3 end module N = M
end;;

module M_pres' : S_pres = M_pres;;
module M_abs' : S_abs = M_abs;;

module M_pres'' : S_pres = M_abs;;
module M_abs'' : S_abs = M_pres;;
[%%expect {|
module type S_pres = sig module M : sig val x : int end module N = M end
module type S_abs = sig module M : sig val x : int end module N == M end
module M_abs : sig module M : sig val x : int end module N == M end
module M_pres : sig module M : sig val x : int end module N = M end
module M_pres' : S_pres
module M_abs' : S_abs
module M_pres'' : S_pres
module M_abs'' : S_abs
|}]

(* Present and absent aliases are incompatible in signatures. *)
type t_pres = (module S_pres);;
type t_abs = (module S_abs);;
let same (x : t_pres) (y : t_abs) b = if b then x else y;; (* Fail *)
[%%expect {|
type t_pres = (module S_pres)
type t_abs = (module S_abs)
Line 3, characters 55-56:
3 | let same (x : t_pres) (y : t_abs) b = if b then x else y;; (* Fail *)
                                                           ^
Error: This expression has type t_abs = (module S_abs)
       but an expression was expected of type t_pres = (module S_pres)
|}]

module Use_pres : sig module type S = S_pres end =
  struct module type S = S_abs end;;
[%%expect {|
Line 2, characters 2-34:
2 |   struct module type S = S_abs end;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type S = S_abs end
       is not included in
         sig module type S = S_pres end
       Module type declarations do not match:
         module type S = S_abs
       does not match
         module type S = S_pres
       At position module type S = <here>
       Illegal permutation of runtime components in a module type.
|}]
module Use_abs : sig module type S = S_abs end =
  struct module type S = S_pres end;;
[%%expect {|
Line 2, characters 2-35:
2 |   struct module type S = S_pres end;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type S = S_pres end
       is not included in
         sig module type S = S_abs end
       Module type declarations do not match:
         module type S = S_pres
       does not match
         module type S = S_abs
       At position module type S = <here>
       Illegal permutation of runtime components in a module type.
|}]
