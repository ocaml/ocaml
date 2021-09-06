(* TEST
   * expect
*)

(* Acription to restrict a module's type. *)
module type Restrict = sig
  module M : sig type t end
  module M' = (M :> sig end)
end

[%%expect {|
module type Restrict =
  sig module M : sig type t end module M' = (M :> sig end) end
|}]

(* Ascription fails when the target type is incompatible. *)
module type Failed_restrict = sig
  module M : sig type t end
  module M' = (M :> sig type u end)
end

[%%expect {|
Line 3, characters 14-35:
3 |   module M' = (M :> sig type u end)
                  ^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type u end
       The type `u' is required but not provided
|}]

(* Ascription can nest. *)
module type Restrict_nested = sig
  module M : sig type t type u end
  module M' = (M :> sig type t end)
  module M'' = (M' :> sig end)
end

[%%expect {|
module type Restrict_nested =
  sig
    module M : sig type t type u end
    module M' = (M :> sig type t end)
    module M'' = (M' :> sig end)
  end
|}]

(* Ascription cannot re-expand. *)
module type Failed_estrict_reexpand = sig
  module M : sig type t type u end
  module M' = (M :> sig type t end)
  module M'' = (M' :> sig type u end)
end

[%%expect {|
Line 4, characters 15-37:
4 |   module M'' = (M' :> sig type u end)
                   ^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type u end
       The type `u' is required but not provided
|}]

(* Acription can restrict values. *)
module type Restrict_values = sig
  module M : sig type t val create : unit -> t end
  module M' = (M :> sig type t end)
end

[%%expect {|
module type Restrict_values =
  sig
    module M : sig type t val create : unit -> t end
    module M' = (M :> sig type t end)
  end
|}]

(* Ascription can restrict submodules. *)
module type Restrict_submodules = sig
  module M : sig
    module N : sig
      type t
      type u
      val create_t : unit -> t
      val create_u : unit -> u
    end
  end
  module M' = (M :> sig
    module N : sig
      type t
      val create_t : unit -> t
    end
  end)
  module N' = M'.N
  module N'' : module type of M'.N
end

[%%expect {|
module type Restrict_submodules =
  sig
    module M :
      sig
        module N :
          sig
            type t
            type u
            val create_t : unit -> t
            val create_u : unit -> u
          end
      end
    module M' = (M :>
      sig module N : sig type t val create_t : unit -> t end end)
    module N' = M'.N
    module N'' : sig type t = M.N.t val create_t : unit -> t end
  end
|}]

(* Ascription can restrict concrete modules. *)
module Concrete = struct
  type t = int
  module N = struct
    type u = t
    type t = bool
    let create_t () : t = false
    let create_u () : u = 0
  end
end
module type Restrict_concrete = sig
  module M = (Concrete :> sig
    module N : sig
      type t
      val create_t : unit -> t
    end
  end)
  module M' : module type of M
end

[%%expect {|
module Concrete :
  sig
    type t = int
    module N :
      sig
        type u = t
        type t = bool
        val create_t : unit -> t
        val create_u : unit -> u
      end
  end
module type Restrict_concrete =
  sig
    module M = (Concrete :>
      sig module N : sig type t val create_t : unit -> t end end)
    module M' :
      sig
        module N : sig type t = Concrete.N.t val create_t : unit -> t end
      end
  end
|}]

(* Ascription can restrict to a module type identifier. *)
module type S_full = sig
  type t
  type u
  val create_t : unit -> t
  val create_u : unit -> u
end
module type S = sig
  type t
  val create_t : unit -> t
end
module M = struct
  type t = int
  type u = bool
  let create_t () = 1
  let create_u () = false
end
module M' : S_full = M
module N : sig module N = (M :> S) end = struct module N = M end
module N' : sig module N = (M' :> S) end = struct module N = M' end

[%%expect {|
module type S_full =
  sig type t type u val create_t : unit -> t val create_u : unit -> u end
module type S = sig type t val create_t : unit -> t end
module M :
  sig
    type t = int
    type u = bool
    val create_t : unit -> int
    val create_u : unit -> bool
  end
module M' : S_full
module N : sig module N = (M :> S) end
module N' : sig module N = (M' :> S) end
|}]

(* Ascription can restrict to a module type of. *)
module M = struct
  type t = int
  type u = bool
  let create_t () = 1
  let create_u () = false
  module N = struct
    type t = string
    let create_t () = ""
  end
end
module M' : S_full = M
module N : sig module N = (M :> module type of M) end = struct module N = M end
module N' : sig module N = (M' :> module type of M') end = struct module N = M' end

[%%expect {|
module M :
  sig
    type t = int
    type u = bool
    val create_t : unit -> int
    val create_u : unit -> bool
    module N : sig type t = string val create_t : unit -> string end
  end
module M' : S_full
module N :
  sig
    module N = (M :>
      sig
        type t = int
        type u = bool
        val create_t : unit -> int
        val create_u : unit -> bool
        module N : sig type t = string val create_t : unit -> string end
      end)
  end
module N' : sig module N = (M' :> S_full) end
|}]

(* Ascription is transparent to types. *)
let m : M.t = N.N.create_t ()
let m' : M'.t = N'.N.create_t ()

[%%expect {|
val m : M.t = 1
val m' : M'.t = <abstr>
|}]

(* Aliases are convertable to ascriptions. *)
module M : sig
  module A : sig type t end
  module B = (A :> sig type t end)
  module C = (B :> sig type t end)
end = struct
  module A = struct type t end
  module B = A
  module C = B
end

[%%expect {|
module M :
  sig
    module A : sig type t end
    module B = (A :> sig type t end)
    module C = (B :> sig type t end)
  end
|}]

(* Ascriptions are convertable to aliases. *)
module M' : sig
  module A : sig type t end
  module B = A
  module C = B
end = M

[%%expect {|
module M' : sig module A : sig type t end module B = A module C = B end
|}]

(* Ascriptions are transparent to module types. *)
module type M = sig
  type outer
  module type S = sig
    type t = outer
    type u
    val create_t : unit -> t
    val create_u : unit -> u
  end
end

module M : sig
  include M
  include S
end = struct
  type outer = int
  module type S = sig
    type t = outer
    type u
    val create_t : unit -> t
    val create_u : unit -> u
  end
  type t = outer
  type u = bool
  let create_t () = 1
  let create_u () = true
end
module N : sig
  module N = (M :> sig
    type outer
    module type S = sig
      type t = outer
      type u
      val create_t : unit -> t
      val create_u : unit -> u
    end
  end)
end = struct module N = M end
module X : N.N.S = struct
  include M
end

[%%expect {|
module type M =
  sig
    type outer
    module type S =
      sig
        type t = outer
        type u
        val create_t : unit -> t
        val create_u : unit -> u
      end
  end
module M :
  sig
    type outer
    module type S =
      sig
        type t = outer
        type u
        val create_t : unit -> t
        val create_u : unit -> u
      end
    type t = outer
    type u
    val create_t : unit -> t
    val create_u : unit -> u
  end
module N :
  sig
    module N = (M :>
      sig
        type outer
        module type S =
          sig
            type t = outer
            type u
            val create_t : unit -> t
            val create_u : unit -> u
          end
      end)
  end
module X : N.N.S
|}]

(* Ascriptions cannot replace aliases in module types.
   (Ensures consistent presence)
*)
module type S = sig type t end
module M : sig
  module type S = sig
    module A : S
    module B = (A :> S)
  end
end = struct
  module type S = sig
    module A : S
    module B = A
  end
end

[%%expect {|
module type S = sig type t end
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |   module type S = sig
 9 |     module A : S
10 |     module B = A
11 |   end
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig module A : S module B = A end end
       is not included in
         sig module type S = sig module A : S module B = (A :> S) end end
       Module type declarations do not match:
         module type S = sig module A : S/2 module B = A end
       does not match
         module type S = sig module A : S/2 module B = (A :> S/2) end
       At position module type S = <here>
       Illegal permutation of runtime components in a module type.
|}]

(* Aliases cannot replace ascriptions in module types.
   (Ensures consistent presence)
*)
module type S = sig type t end
module M : sig
  module type S = sig
    module A : S
    module B = A
  end
end = struct
  module type S = sig
    module A : S
    module B = (A :> S)
  end
end

[%%expect {|
module type S = sig type t end
Lines 7-12, characters 6-3:
 7 | ......struct
 8 |   module type S = sig
 9 |     module A : S
10 |     module B = (A :> S)
11 |   end
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig module A : S module B = (A :> S) end end
       is not included in
         sig module type S = sig module A : S module B = A end end
       Module type declarations do not match:
         module type S = sig module A : S/2 module B = (A :> S/2) end
       does not match
         module type S = sig module A : S/2 module B = A end
       At position module type S = <here>
       Illegal permutation of runtime components in a module type.
|}]

(* Packed modules can coerce between ascriptions and aliases. *)
module type Alias = sig
  module A : S
  module B = A
end
module type Ascription = sig
  module A : S
  module B = (A :> S)
end
let there (module M : Alias) = (module M : Ascription)
let back (module M : Ascription) = (module M : Alias)

[%%expect {|
module type Alias = sig module A : S module B = A end
module type Ascription = sig module A : S module B = (A :> S) end
val there : (module Alias) -> (module Ascription) = <fun>
val back : (module Ascription) -> (module Alias) = <fun>
|}]

(* Packed modules operate correctly after conversion. *)

module type S = sig
  type t
  val create : unit -> t
  val to_int : t -> int
end
module type Alias = sig
  module A : S with type t = int
  module B = A
  module C : S with type t = unit -> int
end
module type Ascription = sig
  module A : S with type t = int
  module B = (A :> S)
  module C : S with type t = unit -> int
end
let there (module M : Alias) = (module M : Ascription)
let back (module M : Ascription) = (module M : Alias)
module Instance : Alias = struct
  module A = struct
    type t = int
    let create () = 0
    let to_int t = t
  end
  module B = A
  module C = struct
    type t = unit -> int
    let create () () = 1
    let to_int t = t ()
  end
end
let res =
  let packed = (module Instance : Alias) in
  let (module M) = packed in
  let (module N) = there packed in
  let (module O) = back (there packed) in
  ( M.A.(create () |> to_int)
  , M.B.(create () |> to_int)
  , M.C.(create () |> to_int)
  , N.A.(create () |> to_int)
  , N.B.(create () |> to_int)
  , N.C.(create () |> to_int)
  , O.A.(create () |> to_int)
  , O.B.(create () |> to_int)
  , O.C.(create () |> to_int) )

[%%expect{|
module type S = sig type t val create : unit -> t val to_int : t -> int end
module type Alias =
  sig
    module A :
      sig type t = int val create : unit -> t val to_int : t -> int end
    module B = A
    module C :
      sig
        type t = unit -> int
        val create : unit -> t
        val to_int : t -> int
      end
  end
module type Ascription =
  sig
    module A :
      sig type t = int val create : unit -> t val to_int : t -> int end
    module B = (A :> S)
    module C :
      sig
        type t = unit -> int
        val create : unit -> t
        val to_int : t -> int
      end
  end
val there : (module Alias) -> (module Ascription) = <fun>
val back : (module Ascription) -> (module Alias) = <fun>
module Instance : Alias
val res : int * int * int * int * int * int * int * int * int =
  (0, 0, 1, 0, 0, 1, 0, 0, 1)
|}]

(* Aliases to ascriptions can unify with aliases to the root module. *)
module M : sig
  module A : sig type t end
  module B = A
  module C = (B :> sig type t end)
  module D = C
  module E = (D :> sig type t end)
  module F = (E :> sig type t end)
end = struct
  module A = struct type t end
  module B = A
  module C = B
  module D = C
  module E = D
  module F = E
end
module N : sig
  module A : sig type t end
  module B = A
  module C = A
  module D = A
  module E = A
  module F = A
end = M
module O : sig
  module A : sig type t end
  module B = A
  module C = (B :> sig type t end)
  module D = C
  module E = (D :> sig type t end)
  module F = (E :> sig type t end)
end = N

[%%expect {|
module M :
  sig
    module A : sig type t end
    module B = A
    module C = (B :> sig type t end)
    module D = C
    module E = (D :> sig type t end)
    module F = (E :> sig type t end)
  end
module N :
  sig
    module A : sig type t end
    module B = A
    module C = A
    module D = A
    module E = A
    module F = A
  end
module O :
  sig
    module A : sig type t end
    module B = A
    module C = (B :> sig type t end)
    module D = C
    module E = (D :> sig type t end)
    module F = (E :> sig type t end)
  end
|}]

(* Aliases to incompatible ascriptions do not unify with aliases to the root module. *)
module M : sig
  module A : sig type t type u end
  module B = A
  module C = (B :> sig type t type u end)
  module D = C
  module E = (D :> sig type t end)
  module F = (E :> sig end)
end = struct
  module A = struct type t type u end
  module B = A
  module C = B
  module D = C
  module E = D
  module F = E
end
module N : sig
  module A : sig type t end
  module B = A
  module C = A
  module D = A
  module E = A
  module F = A
end = M

[%%expect {|
module M :
  sig
    module A : sig type t type u end
    module B = A
    module C = (B :> sig type t type u end)
    module D = C
    module E = (D :> sig type t end)
    module F = (E :> sig end)
  end
Line 23, characters 6-7:
23 | end = M
           ^
Error: Signature mismatch:
       Modules do not match:
         sig
           module A = M.A
           module B = A
           module C = M.C
           module D = C
           module E = M.E
           module F = M.F
         end
       is not included in
         sig
           module A : sig type t end
           module B = A
           module C = A
           module D = A
           module E = A
           module F = A
         end
       In module E:
       Modules do not match: (module M.E) is not included in (module A)
       In module E:
       Modules aliases do not match:
         sig type t = M.A.t end
       is not equal to
         sig type t = M.A.t type u = M.A.u end
       The first module is not included in the second
       In module E:
       Module types do not match:
         sig type t = M.A.t end
       is not equal to
         sig type t = M.A.t type u = M.A.u end
       In module E:
       The type `u' is required but not provided
|}]



(* Aliases to functor arguments are ascribed. *)
module F (A : sig type t end) = struct
  module A = A
end

[%%expect {|
module F :
  functor (A : sig type t end) -> sig module A = (A :> sig type t end) end
|}]

(* Aliases within functor arguments are ascribed. *)
module F (A : sig module M : sig type t end end) = struct
  type t = A.M.t
  module B = A.M
  module A = A
end
module F' : functor (A : sig module M : sig type t end end) -> sig
  type t = A.M.t
  module A = (A :> sig module M : sig type t end end)
  module B = A.M
end = F

[%%expect {|
module F :
  functor (A : sig module M : sig type t end end) ->
    sig
      type t = A.M.t
      module B = (A.M :> sig type t end)
      module A = (A :> sig module M : sig type t end end)
    end
module F' :
  functor (A : sig module M : sig type t end end) ->
    sig
      type t = A.M.t
      module A = (A :> sig module M : sig type t end end)
      module B = A.M
    end
|}]

(* Ascriptions as functor arguments are transparent to type resolution. *)
module A : sig
  module N : sig
    module M : sig type t end
  end
  module O = (N :> sig
    module M : sig type t end
  end)
end = struct
  module N = struct
    module M = struct type t end
  end
  module O = N
end
type t = F(A.N).t
type u = F(A.O).t
let f (t : t) (u : u) : u * t = (t, u)

[%%expect {|
module A :
  sig
    module N : sig module M : sig type t end end
    module O = (N :> sig module M : sig type t end end)
  end
type t = F(A.N).t
type u = F(A.O).t
val f : t -> u -> u * t = <fun>
|}]

(* Narrowing ascriptions as functor arguments are transparent to type
   resolution.
*)
module A : sig
  module N : sig
    module M : sig type t end
    module O : sig type t end
  end
  module O = (N :> sig
    module M : sig type t end
  end)
end = struct
  module N = struct
    module M = struct type t end
    module O = struct type t end
  end
  module O = N
end
type t = F(A.N).t
type u = F(A.O).t
let f (t : t) (u : u) : u * t = (t, u)

[%%expect {|
module A :
  sig
    module N : sig module M : sig type t end module O : sig type t end end
    module O = (N :> sig module M : sig type t end end)
  end
type t = F(A.N).t
type u = F(A.O).t
val f : t -> u -> u * t = <fun>
|}]

(* Ascription syntax generates an ascription type. *)
module M = struct
  module A = struct
    type t = int
    let create () = 15
  end
  module B = struct
    type t = unit -> int
    let create () () = 15
  end
  module C = B
end
module N = (M :> sig module A : sig type t val create : unit -> t end end)
module type S = sig module B : sig type t val create : unit -> t end end
module O = (M :> S)
module P = (M :> sig module C = M.B end)

[%%expect {|
module M :
  sig
    module A : sig type t = int val create : unit -> int end
    module B : sig type t = unit -> int val create : unit -> unit -> int end
    module C = B
  end
module N = (M :> sig module A : sig type t val create : unit -> t end end)
module type S = sig module B : sig type t val create : unit -> t end end
module O = (M :> S)
module P = (M :> sig module C = M.B end)
|}]

(* Ascription syntax as functor arguments. *)
module F (A : S) : S = A
module F_N = F((M :> S))
module F' (A : S) = A
module F'_N = F'((M :> S))

[%%expect {|
module F : functor (A : S) -> S
module F_N :
  sig module B : sig type t = F(M).B.t val create : unit -> t end end
module F' :
  functor (A : S) ->
    sig module B = (A.B :> sig type t val create : unit -> t end) end
module F'_N :
  sig module B = (M.B :> sig type t val create : unit -> t end) end
|}]
