(* TEST
   * expect
*)

module C = Char;;
C.chr 66;;

module C' : module type of Char = C;;
C'.chr 66;;

module C3 = struct include Char end;;
C3.chr 66;;
[%%expect{|
module C = Char
- : char = 'B'
module C' :
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
module C3 :
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

let f x = let module M = struct module L = List end in M.L.length x;;
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

module G(X:sig end) = struct module M = X end;; (* (weak) aliases to X *)
module M = G(struct end);;
[%%expect{|
module G : functor (X : sig end) -> sig module M = X end
module M : sig module M : sig end end
|}];;

module M' = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M'.N'.x;;
[%%expect{|
module M' : sig module N : sig val x : int end module N' = N end
- : int = 1
|}];;

module M'' : sig module N' : sig val x : int end end = M';;
M''.N'.x;;
module M2 = struct include M' end;;
module M3 : sig module N' : sig val x : int end end = struct include M' end;;
M3.N'.x;;
module M3' : sig module N' : sig val x : int end end = M2;;
M3'.N'.x;;
[%%expect{|
module M'' : sig module N' : sig val x : int end end
- : int = 1
module M2 : sig module N = M'.N module N' = N end
module M3 : sig module N' : sig val x : int end end
- : int = 1
module M3' : sig module N' : sig val x : int end end
- : int = 1
|}];;

module M4 : sig module N' : sig val x : int end end = struct
  module N = struct let x = 1 end
  module N' = N
end;;
M4.N'.x;;
[%%expect{|
module M4 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module F(X:sig end) = struct
  module N = struct let x = 1 end
  module N' = N
end;;
module G : functor(X:sig end) -> sig module N' : sig val x : int end end = F;;
module M5 = G(struct end);;
M5.N'.x;;
[%%expect{|
module F :
  functor (X : sig end) ->
    sig module N : sig val x : int end module N' = N end
module G : functor (X : sig end) -> sig module N' : sig val x : int end end
module M5 : sig module N' : sig val x : int end end
- : int = 1
|}];;

module M = struct
  module D = struct let y = 3 end
  module N = struct let x = 1 end
  module N' = N
end;;

module M1 : sig module N : sig val x : int end module N' = N end = M;;
M1.N'.x;;
module M2 : sig module N' : sig val x : int end end =
  (M : sig module N : sig val x : int end module N' = N end);;
M2.N'.x;;

open M;;
N'.x;;
[%%expect{|
module M :
  sig
    module D : sig val y : int end
    module N : sig val x : int end
    module N' = N
  end
module M1 : sig module N : sig val x : int end module N' = N end
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
  : sig module C : sig val escaped : char -> string end module C' = C end
  = M;; (* sound, but should probably fail *)
M1.C'.escaped 'A';;
module M2 : sig module C' : sig val chr : int -> char end end =
  (M : sig module C : sig val chr : int -> char end module C' = C end);;
M2.C'.chr 66;;
[%%expect{|
module M : sig module C = Char module C' = C end
module M1 :
  sig module C : sig val escaped : char -> string end module C' = C end
- : string = "A"
module M2 : sig module C' : sig val chr : int -> char end end
- : char = 'B'
|}];;

StdLabels.List.map;;
[%%expect{|
- : f:('a -> 'b) -> 'a list -> 'b list = <fun>
|}];;

module Q = Queue;;
exception QE = Q.Empty;;
try Q.pop (Q.create ()) with QE -> "Ok";;
[%%expect{|
module Q = Queue
exception QE
- : string = "Ok"
|}];;

module type Complex = module type of Complex with type t = Complex.t;;
module M : sig module C : Complex end = struct module C = Complex end;;

module C = Complex;;
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
module C = Complex
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

module F(X:sig module C = Char end) = struct module C = X.C end;;
[%%expect{|
module F : functor (X : sig module C = Char end) -> sig module C = X.C end
|}];;

(* Applicative functors *)
module S = String
module StringSet = Set.Make(String)
module SSet = Set.Make(S);;
let f (x : StringSet.t) = (x : SSet.t);;
[%%expect{|
module S = String
module StringSet = Set.Make(String)
module SSet = Set.Make(S)
val f : StringSet.t -> SSet.t = <fun>
|}];;

(* Also using include (cf. Leo's mail 2013-11-16) *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
  module M = struct end
  include F(M)
end;;
include T;;
let f (x : t) : T.t = x ;;
[%%expect{|
module F : functor (M : sig end) -> sig type t end
module T : sig module M : sig end type t = F(M).t end
module M = T.M
type t = F(M).t
val f : t -> T.t = <fun>
|}];;

(* PR#4049 *)
(* This works thanks to abbreviations *)
module A = struct
  module B = struct type t let compare x y = 0 end
  module S = Set.Make(B)
  let empty = S.empty
end
module A1 = A;;
A1.empty = A.empty;;
[%%expect{|
module A :
  sig
    module B : sig type t val compare : 'a -> 'b -> int end
    module S = Set.Make(B)
    val empty : S.t
  end
module A1 = A
- : bool = true
|}];;

(* PR#3476: *)
module FF(X : sig end) = struct type t end
module M = struct
  module X = struct end
  module Y = FF (X)
  type t = Y.t
end
module F (Y : sig type t end) (M : sig type t = Y.t end) = struct end;;

module G = F (M.Y);;
module N = G (M);;
module N = F (M.Y) (M);;
[%%expect{|
module FF : functor (X : sig end) -> sig type t end
module M : sig module X : sig end module Y = FF(X) type t = Y.t end
module F : functor (Y : sig type t end) (M : sig type t = Y.t end) -> sig end
module G = F(M.Y)
module N = G(M)
module N = F(M.Y)(M)
|}];;

(* PR#5058 *)
module F (M : sig end) : sig type t end = struct type t = int end
module T = struct
module M = struct end
include F(M)
end
include T
let f (x : t) : T.t = x
[%%expect {|
module F : functor (M : sig end) -> sig type t end
module T : sig module M : sig end type t = F(M).t end
module M = T.M
type t = F(M).t
val f : t -> T.t = <fun>
|}]

(* PR#6307 *)

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
module L1 : sig module X = A1 end
module L2 : sig module X = A2 end
module F : functor (L : sig module X : sig end end) -> sig end
module F1 = F(L1)
module F2 = F(L2)
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
  module I = Int2
  include S with module I := I
end;; (* fail *)
[%%expect{|
module Int : sig type t = int val compare : 'a -> 'a -> int end
module SInt = Set.Make(Int)
type (_, _) eq = Eq : ('a, 'a) eq
type wrap = W of (SInt.t, SInt.t) eq
module M :
  sig
    module I = Int
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(I).t) eq
  end
module type S =
  sig
    module I = Int
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
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I = Int end
    module P : sig module I = N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
  end
module type S =
  sig
    module N : sig module I = Int end
    module P : sig module I = N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(P.I).t) eq end
  end
|}];;

module M = struct
  module N = struct module I = Int end
  module P = struct module I = N.I end
  module Q = struct
    type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq
  end
end;;
module type S = module type of M [@remove_aliases];;
[%%expect{|
module M :
  sig
    module N : sig module I = Int end
    module P : sig module I = N.I end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
  end
module type S =
  sig
    module N : sig module I = Int end
    module P :
      sig module I : sig type t = int val compare : 'a -> 'a -> int end end
    module Q :
      sig type wrap' = wrap = W of (Set.Make(Int).t, Set.Make(N.I).t) eq end
  end
|}];;

(* PR#6365 *)
module type S = sig module M : sig type t val x : t end end;;
module H = struct type t = A let x = A end;;
module H' = H;;
module type S' = S with module M = H';; (* shouldn't introduce an alias *)
[%%expect{|
module type S = sig module M : sig type t val x : t end end
module H : sig type t = A val x : t end
module H' = H
module type S' = sig module M : sig type t = H.t = A val x : t end end
|}];;

(* PR#6376 *)
module type Alias = sig module N : sig end module M = N end;;
module F (X : sig end) = struct type t end;;
module type A = Alias with module N := F(List);;
module rec Bad : A = Bad;;
[%%expect{|
module type Alias = sig module N : sig end module M = N end
module F : functor (X : sig end) -> sig type t end
Line 1:
Error: Module type declarations do not match:
         module type A = sig module M = F(List) end
       does not match
         module type A = sig module M = F(List) end
       At position module type A = <here>
       Module types do not match:
         sig module M = F(List) end
       is not equal to
         sig module M = F(List) end
       At position module type A = sig module M : <here> end
       Module F(List) cannot be aliased
|}];;

(* Shinwell 2014-04-23 *)
module B = struct
 module R = struct
   type t = string
 end

 module O = R
end

module K = struct
 module E = B
 module N = E.O
end;;

let x : K.N.t = "foo";;
[%%expect{|
module B : sig module R : sig type t = string end module O = R end
module K : sig module E = B module N = E.O end
val x : K.N.t = "foo"
|}];;

(* PR#6465 *)

module M = struct type t = A module B = struct type u = B end end;;
module P : sig type t = M.t = A module B = M.B end = M;;
module P : sig type t = M.t = A module B = M.B end = struct include M end;;
[%%expect{|
module M : sig type t = A module B : sig type u = B end end
module P : sig type t = M.t = A module B = M.B end
module P : sig type t = M.t = A module B = M.B end
|}];;

module type S = sig
  module M : sig module P : sig end end
  module Q = M
end;;
[%%expect{|
module type S = sig module M : sig module P : sig end end module Q = M end
|}];;
module type S = sig
  module M : sig module N : sig end module P : sig end end
  module Q : sig module N = M.N module P = M.P end
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
    module Q : sig module N = M.N module P = M.P end
  end
module R :
  sig
    module M : sig module N : sig end module P : sig end end
    module Q = M
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
  struct module M = M end;;
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

module Bar : S with module M := Foo = struct module N = Foo.A end

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

module N = M.F(struct module A = M.N.A end)

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

(* Check that codegen works correctly with weak aliases.
   These tests should segfault (by trying to dereference 0) if the modules
   become misaligned due to a bug with handling of module presence. *)
module type S = sig
  type t
  val create : unit -> t
end
module INT = struct
  type t = int
  let create () = 0
end
module REF = struct
  type t = int ref ref
  let create () = ref (ref 1)
end
module F (X : S) = struct
  module A = REF
  module B = REF
  module C = REF
  module D = X
  module E = REF
  module F = REF
  module G = REF
end
module M = F(INT)
let sum_m =
    ! !(M.A.create ())
  + ! !(M.B.create ())
  + ! !(M.C.create ())
  + ! !(M.E.create ())
  + ! !(M.F.create ())
  + ! !(M.G.create ())
module N : sig (* Force REF-aliased modules to be present *)
  module A : S with type t = int ref ref
  module B : S with type t = int ref ref
  module C : S with type t = int ref ref
  module D = INT
  module E : S with type t = int ref ref
  module F : S with type t = int ref ref
  module G : S with type t = int ref ref
end = M
let sum_n =
    ! !(N.A.create ())
  + ! !(N.B.create ())
  + ! !(N.C.create ())
  + ! !(N.E.create ())
  + ! !(N.F.create ())
  + ! !(N.G.create ())
module O : sig (* Force all modules to be present *)
  module A : S with type t = int ref ref
  module B : S with type t = int ref ref
  module C : S with type t = int ref ref
  module D : S with type t = int
  module E : S with type t = int ref ref
  module F : S with type t = int ref ref
  module G : S with type t = int ref ref
end = M
let sum_o =
    ! !(O.A.create ())
  + ! !(O.B.create ())
  + ! !(O.C.create ())
  + ! !(O.E.create ())
  + ! !(O.F.create ())
  + ! !(O.G.create ())
module P : sig (* Force INT module to be present *)
  module A = REF
  module B = REF
  module C = REF
  module D : S with type t = int
  module E = REF
  module F = REF
  module G = REF
end = M
let sum_p =
    ! !(P.A.create ())
  + ! !(P.B.create ())
  + ! !(P.C.create ())
  + ! !(P.E.create ())
  + ! !(P.F.create ())
  + ! !(P.G.create ())
module F' (X : S) : sig (* Functor with all modules present *)
  module A : S with type t = int ref ref
  module B : S with type t = int ref ref
  module C : S with type t = int ref ref
  module D : S with type t = X.t
  module E : S with type t = int ref ref
  module F : S with type t = int ref ref
  module G : S with type t = int ref ref
end = struct
  module A = REF
  module B = REF
  module C = REF
  module D = X
  module E = REF
  module F = REF
  module G = REF
end
module Q = F'(INT)
let sum_q =
    ! !(Q.A.create ())
  + ! !(Q.B.create ())
  + ! !(Q.C.create ())
  + ! !(Q.E.create ())
  + ! !(Q.F.create ())
  + ! !(Q.G.create ())
module F'' (X : S) : sig (* Functor with REF-aliased modules present *)
  module A : S with type t = int ref ref
  module B : S with type t = int ref ref
  module C : S with type t = int ref ref
  module D = X
  module E : S with type t = int ref ref
  module F : S with type t = int ref ref
  module G : S with type t = int ref ref
end = struct
  module A = REF
  module B = REF
  module C = REF
  module D = X
  module E = REF
  module F = REF
  module G = REF
end
module R = F''(INT)
let sum_r =
    ! !(R.A.create ())
  + ! !(R.B.create ())
  + ! !(R.C.create ())
  + ! !(R.E.create ())
  + ! !(R.F.create ())
  + ! !(R.G.create ());;

[%%expect {|
module type S = sig type t val create : unit -> t end
module INT : sig type t = int val create : unit -> int end
module REF : sig type t = int ref ref val create : unit -> int ref ref end
module F :
  functor (X : S) ->
    sig
      module A = REF
      module B = REF
      module C = REF
      module D = X
      module E = REF
      module F = REF
      module G = REF
    end
module M = F(INT)
val sum_m : int = 6
module N :
  sig
    module A : sig type t = int ref ref val create : unit -> t end
    module B : sig type t = int ref ref val create : unit -> t end
    module C : sig type t = int ref ref val create : unit -> t end
    module D = INT
    module E : sig type t = int ref ref val create : unit -> t end
    module F : sig type t = int ref ref val create : unit -> t end
    module G : sig type t = int ref ref val create : unit -> t end
  end
val sum_n : int = 6
module O :
  sig
    module A : sig type t = int ref ref val create : unit -> t end
    module B : sig type t = int ref ref val create : unit -> t end
    module C : sig type t = int ref ref val create : unit -> t end
    module D : sig type t = int val create : unit -> t end
    module E : sig type t = int ref ref val create : unit -> t end
    module F : sig type t = int ref ref val create : unit -> t end
    module G : sig type t = int ref ref val create : unit -> t end
  end
val sum_o : int = 6
module P :
  sig
    module A = REF
    module B = REF
    module C = REF
    module D : sig type t = int val create : unit -> t end
    module E = REF
    module F = REF
    module G = REF
  end
val sum_p : int = 6
module F' :
  functor (X : S) ->
    sig
      module A : sig type t = int ref ref val create : unit -> t end
      module B : sig type t = int ref ref val create : unit -> t end
      module C : sig type t = int ref ref val create : unit -> t end
      module D : sig type t = X.t val create : unit -> t end
      module E : sig type t = int ref ref val create : unit -> t end
      module F : sig type t = int ref ref val create : unit -> t end
      module G : sig type t = int ref ref val create : unit -> t end
    end
module Q = F'(INT)
val sum_q : int = 6
module F'' :
  functor (X : S) ->
    sig
      module A : sig type t = int ref ref val create : unit -> t end
      module B : sig type t = int ref ref val create : unit -> t end
      module C : sig type t = int ref ref val create : unit -> t end
      module D = X
      module E : sig type t = int ref ref val create : unit -> t end
      module F : sig type t = int ref ref val create : unit -> t end
      module G : sig type t = int ref ref val create : unit -> t end
    end
module R = F''(INT)
val sum_r : int = 6
|}]

(* Functor aliases *)
module M : sig
  module A = F(INT).A
  module B = F'(INT).B
  module C = F''(INT).C
  module D = F(INT).D
  module E = F'(INT).D (* Not a typo: we want the functor argument's alias. *)
  module F = F''(INT).D (* Not a typo: we want the functor argument's alias. *)
end = struct
  module A = M.A
  module B = Q.B
  module C = R.C
  module D = M.D
  module E = Q.D
  module F = R.D
end
module N : sig (* Refine from weak aliases to abstract types. *)
  module A : S
  module B : S
  module C : S
  module D : S
  module E : S
  module F : S
end = M;;
[%%expect {|
module M :
  sig
    module A = REF
    module B = F'(INT).B
    module C = F''(INT).C
    module D = INT
    module E = F'(INT).D
    module F = INT
  end
module N :
  sig
    module A : S
    module B : S
    module C : S
    module D : S
    module E : S
    module F : S
  end
|}]
