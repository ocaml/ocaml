(* TEST
   * expect
*)

(* Shadows aliases.ml, with absent modules as appropriate. *)

module C == Char;;
C.chr 66;;

[%%expect{|
module C == Char
- : char = 'B'
|}];;

let f x = let module M = struct module L == List end in M.L.length x;;
let g x = let module L = List in L.length (L.map succ x);;
[%%expect{|
val f : 'a list -> int = <fun>
val g : int list -> int = <fun>
|}];;

module F(X:sig end) = Char;;
module C4 = F(struct end);;
C4.chr 66;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig
      external code : char -> int = "%identity"
      val chr : int -> char
      val escaped : char -> string
      val lowercase : char -> char
      val uppercase : char -> char
      val lowercase_ascii : char -> char
      val uppercase_ascii : char -> char
      type t = char
      val compare : t -> t -> int
      val equal : t -> t -> bool
      external unsafe_chr : int -> char = "%identity"
    end
module C4 :
  sig
    external code : char -> int = "%identity"
    val chr : int -> char
    val escaped : char -> string
    val lowercase : char -> char
    val uppercase : char -> char
    val lowercase_ascii : char -> char
    val uppercase_ascii : char -> char
    type t = char
    val compare : t -> t -> int
    val equal : t -> t -> bool
    external unsafe_chr : int -> char = "%identity"
  end
- : char = 'B'
|}];;

module G(X:sig end) = struct module M == X end;; (* does not alias X *)
[%%expect{|
Line 1, characters 41-42:
1 | module G(X:sig end) = struct module M == X end;; (* does not alias X *)
                                             ^
Error: The module path X cannot be made absent:
       it does not refer to a module with a known location.
|}];;

module M' = struct
  module N = struct let x = 1 end
  module N' == N
end;;
M'.N'.x;;
[%%expect{|
module M' : sig module N : sig val x : int end module N' == N end
- : int = 1
|}];;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' == N
end;;
M4.N'.x;;
[%%expect{|
module M4 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' == N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig module N : sig val x : int end module N' == N end
module G : functor (X : sig end) -> sig module N' : sig val x : int end end
module M5 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' == N
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
  module C == Char
  module C' == C
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
  module A' == A
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

module Q == Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;
[%%expect{|
module Q == Queue
exception QE
- : string = "Ok"
|}];;

module type Complex = module type of Complex with type t = Complex.t;;
module M : sig module C : Complex end = struct module C == Complex end;;

module C == Complex;;
C.one.Complex.re;;
include C;;
[%%expect{|
module type Complex =
  sig
    type t = Complex.t = { re : float; im : float; }
    val zero : t
    val one : t
    val i : t
    val neg : t -> t
    val conj : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val inv : t -> t
    val div : t -> t -> t
    val sqrt : t -> t
    val norm2 : t -> float
    val norm : t -> float
    val arg : t -> float
    val polar : float -> float -> t
    val exp : t -> t
    val log : t -> t
    val pow : t -> t -> t
  end
module M : sig module C : Complex end
module C == Complex
- : float = 1.
type t = Complex.t = { re : float; im : float; }
val zero : t = {re = 0.; im = 0.}
val one : t = {re = 1.; im = 0.}
val i : t = {re = 0.; im = 1.}
val neg : t -> t = <fun>
val conj : t -> t = <fun>
val add : t -> t -> t = <fun>
val sub : t -> t -> t = <fun>
val mul : t -> t -> t = <fun>
val inv : t -> t = <fun>
val div : t -> t -> t = <fun>
val sqrt : t -> t = <fun>
val norm2 : t -> float = <fun>
val norm : t -> float = <fun>
val arg : t -> float = <fun>
val polar : float -> float -> t = <fun>
val exp : t -> t = <fun>
val log : t -> t = <fun>
val pow : t -> t -> t = <fun>
|}];;

module F(X:sig module C == Char end) = struct module C == X.C end;;
[%%expect{|
Line 1, characters 58-61:
1 | module F(X:sig module C == Char end) = struct module C == X.C end;;
                                                              ^^^
Error: The module path X.C cannot be made absent:
       it does not refer to a module with a known location.
|}];;
(* Applicative functors *)
module S == String
module StringSet = Set.Make(String)
[%%expect{|
module S == String
module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
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
|}];;

module A1 = struct end
module A2 = struct end
module L1 = struct module X == A1 end
module L2 = struct module X == A2 end;;

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
  module I == Int
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

(* Check behavior with submodules *)
module M = struct
  module N = struct module I == Int end
  module P = struct module I == N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I == Int end
    module P : sig module I == N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
  end
module type S =
  sig
    module N : sig module I == Int end
    module P : sig module I == N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
  end
|}];;

module M = struct
  module N = struct module I == Int end
  module P = struct module I == N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I == Int end
    module P : sig module I == N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
  end
module type S =
  sig
    module N : sig module I == Int end
    module P :
      sig module I : sig type t = int val compare : 'a -> 'a -> int end end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
  end
|}];;

(* PR#6365 *)
module type S = sig module M : sig type t val x : t end end;;
module H = struct type t = A let x = A end;;
module H' == H;;
module type S' = S with module M = H';; (* shouldn't introduce an alias *)
[%%expect{|
module type S = sig module M : sig type t val x : t end end
module H : sig type t = A val x : t end
module H' == H
module type S' = sig module M : sig type t = H.t = A val x : t end end
|}];;

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

(* Shinwell 2014-04-23 *)
module B = struct
 module R = struct
   type t = string
 end

 module O == R
end

module K = struct
 module E == B
 module N == E.O
end;;

let x : K.N.t = "foo";;
[%%expect{|
module B : sig module R : sig type t = string end module O == R end
module K : sig module E == B module N == E.O end
val x : K.N.t = "foo"
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
  module Q == M
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

module F (X : sig end) = struct type t end;;
module M : sig
  type a
  module Foo : sig
    module Bar : sig end
    type b = a
  end
end = struct
  module Foo = struct
    module Bar = struct end
    type b = F(Bar).t
  end
  type a = Foo.b
end;;
[%%expect{|
module F : functor (X : sig end) -> sig type t end
module M :
  sig type a module Foo : sig module Bar : sig end type b = a end end
|}];;

(* PR#6578 *)

module M = struct let f x = x end
module rec R : sig module M : sig val f : 'a -> 'a end end =
  struct module M == M end;;
R.M.f 3;;
[%%expect{|
module M : sig val f : 'a -> 'a end
module rec R : sig module M : sig val f : 'a -> 'a end end
- : int = 3
|}];;
module rec R : sig module M = M end = struct module M = M end;;
R.M.f 3;;
[%%expect{|
module rec R : sig module M = M end
- : int = 3
|}];;

module M = struct type t end
module type S = sig module N = M val x : N.t end
module type T = S with module N := M;;
[%%expect{|
module M : sig type t end
module type S = sig module N = M val x : N.t end
module type T = sig val x : M.t end
|}];;


module X = struct module N = struct end end
module Y : sig
  module type S = sig module N = X.N end
end = struct
  module type S = module type of struct include X end
end;;
[%%expect{|
module X : sig module N : sig end end
module Y : sig module type S = sig module N = X.N end end
|}];;

module type S = sig
  module M : sig
    module A : sig end
    module B : sig end
  end
  module N = M.A
end

module Foo = struct
  module B = struct let x = 0 end
  module A = struct let x = "hello" end
end

module Bar : S with module M := Foo = struct module N == Foo.A end

let s : string = Bar.N.x
[%%expect {|
module type S =
  sig
    module M : sig module A : sig end module B : sig end end
    module N = M.A
  end
module Foo :
  sig module B : sig val x : int end module A : sig val x : string end end
module Bar : sig module N = Foo.A end
val s : string = "hello"
|}]


module M : sig
  module N : sig
    module A : sig val x : string end
    module B : sig val x : int end
  end
  module F (X : sig module A = N.A end) : sig val s : string end
end = struct
  module N = struct
    module B = struct let x = 0 end
    module A = struct let x = "hello" end
  end
  module F (X : sig module A : sig val x : string end end) = struct
    let s = X.A.x
  end
end

module N = M.F(struct module A == M.N.A end)

let s : string = N.s
[%%expect {|
module M :
  sig
    module N :
      sig
        module A : sig val x : string end
        module B : sig val x : int end
      end
    module F : functor (X : sig module A = N.A end) -> sig val s : string end
  end
module N : sig val s : string end
val s : string = "hello"
|}]



(* Coercion between present and absent aliases. *)
module type S_pres = sig module M : sig val x : int end module N = M end;;
module type S_abs = sig module M : sig val x : int end module N == M end;;
module M_pres = struct module M = struct let x = 2 end module N = M end;;
module M_abs = struct module M = struct let x = 3 end module N == M end;;

module M_pres' : S_pres = M_pres;;
module M_abs' : S_abs = M_abs;;

module M_pres'' : S_pres = M_abs;;
module M_abs'' : S_abs = M_pres;;
[%%expect {|
module type S_pres = sig module M : sig val x : int end module N = M end
module type S_abs = sig module M : sig val x : int end module N == M end
module M_pres : sig module M : sig val x : int end module N = M end
module M_abs : sig module M : sig val x : int end module N == M end
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
