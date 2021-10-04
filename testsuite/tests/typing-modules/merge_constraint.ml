(* TEST
   * expect *)

(* #9623 *)

module RhsScopeCheck = struct
  module type Sig1 = sig
    type t
    type u = t
  end

  (* A scoping error here is intentional:
     with-constraints "with <lhs> = <rhs>"
     have their <rhs> evaluated in the current
     typing environment, not within the signature
     that they are constraining. [t] is unbound
     in the current environment, so [with u = t]
     must be rejected. *)
  module type Check1 = Sig1
    with type u = t
end
[%%expect{|
Line 15, characters 18-19:
15 |     with type u = t
                       ^
Error: Unbound type constructor t
|}]


module VarianceEnv = struct
  module type Sig = sig
    type +'a abstract
    type +'a user = Foo of 'a abstract
  end

  module type Problem = sig
    include Sig
    module M : Sig
      with type 'a abstract = 'a abstract
       and type 'a user = 'a user

    (* the variance annotation of [+'a foo] should be accepted, which
       would not be the case if the with-constraint [and type 'a
       user = 'a user] had its variance type-checked in the wrong typing
       environment: see #9624 *)
    type +'a foo = 'a M.user
  end
end
[%%expect{|
module VarianceEnv :
  sig
    module type Sig =
      sig type +'a abstract type 'a user = Foo of 'a abstract end
    module type Problem =
      sig
        type +'a abstract
        type 'a user = Foo of 'a abstract
        module M :
          sig
            type 'a abstract = 'a abstract
            type 'a user = 'a user = Foo of 'a abstract
          end
        type 'a foo = 'a M.user
      end
  end
|}]

module UnboxedEnv = struct
  module type Sig = sig
    type 'a ind = 'a * int
    type t = T : 'e ind -> t [@@unboxed]
  end

  module type Problem = sig
    include Sig
    module type ReboundSig = Sig
      with type 'a ind = 'a ind
       and type t = t
    (* the with-definition [and type t = t] above should be accepted,
       which would not be the case if its definition had separability
       checked in the wrong typing environment: see #9624 *)
  end
end
[%%expect{|
module UnboxedEnv :
  sig
    module type Sig =
      sig type 'a ind = 'a * int type t = T : 'e ind -> t [@@unboxed] end
    module type Problem =
      sig
        type 'a ind = 'a * int
        type t = T : 'e ind -> t [@@unboxed]
        module type ReboundSig =
          sig
            type 'a ind = 'a ind
            type t = t/2 = T : 'a ind -> t/1 [@@unboxed]
          end
      end
  end
|}]

(* We can also have environment issues when unifying type parameters;
   regression test contributed by Jacques Garrigue in #9623. *)
module ParamsUnificationEnv = struct
  module type Sig =
    sig type 'a u = 'a list type +'a t constraint 'a = 'b u end
  type +'a t = 'b constraint 'a = 'b list
  module type Sig2 = Sig with type +'a t = 'a t
end
[%%expect{|
module ParamsUnificationEnv :
  sig
    module type Sig =
      sig type 'a u = 'a list type +'a t constraint 'a = 'b u end
    type +'a t = 'b constraint 'a = 'b list
    module type Sig2 =
      sig type 'a u = 'a list type +'a t = 'a t constraint 'a = 'b u end
  end
|}]


(* The construction of the "signature environment" was also broken
   in earlier versions of the code. Regression test by Leo White in #9623. *)
module CorrectEnvConstructionTest = struct
  module type Sig = sig
    type +'a user = Foo of 'a abstract
    and +'a abstract
  end

  module type Problem = sig
    include Sig
    module M : Sig
      with type 'a abstract = 'a abstract
       and type 'a user = 'a user
    type +'a foo = 'a M.user
  end
end
[%%expect{|
module CorrectEnvConstructionTest :
  sig
    module type Sig =
      sig type 'a user = Foo of 'a abstract and +'a abstract end
    module type Problem =
      sig
        type 'a user = Foo of 'a abstract
        and +'a abstract
        module M :
          sig
            type 'a user = 'a user = Foo of 'a abstract/1
            and 'a abstract = 'a abstract/2
          end
        type 'a foo = 'a M.user
      end
  end
|}]

(* #9640 *)

module type Packet_type = sig
  type t
end
module type Unpacked_header = sig
  module Packet_type : Packet_type
  type t
  val f : t -> Packet_type.t -> unit
end
module type Header = sig
  module Packet_type : Packet_type
  module Unpacked : Unpacked_header with module Packet_type := Packet_type
end
module type S = sig
  module Packet_type : Packet_type
  module Header : Header with module Packet_type = Packet_type
end
[%%expect{|
module type Packet_type = sig type t end
module type Unpacked_header =
  sig
    module Packet_type : Packet_type
    type t
    val f : t -> Packet_type.t -> unit
  end
module type Header =
  sig
    module Packet_type : Packet_type
    module Unpacked : sig type t val f : t -> Packet_type.t -> unit end
  end
module type S =
  sig
    module Packet_type : Packet_type
    module Header :
      sig
        module Packet_type : sig type t = Packet_type.t end
        module Unpacked : sig type t val f : t -> Packet_type.t -> unit end
      end
  end
|}]
module type Iobuf_packet = sig
  module Make (Header : Header) () :
    S
    with module Packet_type = Header.Packet_type
    with module Header.Unpacked = Header.Unpacked
end
[%%expect{|
module type Iobuf_packet =
  sig
    module Make :
      functor (Header : Header) () ->
        sig
          module Packet_type : sig type t = Header.Packet_type.t end
          module Header :
            sig
              module Packet_type : sig type t = Packet_type.t end
              module Unpacked :
                sig
                  type t = Header.Unpacked.t
                  val f : t -> Header.Packet_type.t -> unit
                end
            end
        end
  end
|}]

(* Simpler example by @gasche *)
module type S = sig
  type t
  type u = t
end
module type Pack = sig
  module M : S
end
[%%expect{|
module type S = sig type t type u = t end
module type Pack = sig module M : S end
|}]
module type Weird = sig
  module M : S
  module P : Pack
    with type M.t = M.t
    with type M.u = M.u
end
[%%expect{|
module type Weird =
  sig
    module M : S
    module P : sig module M : sig type t = M.t type u = M.u end end
  end
|}]

(* Recursion issues *)

(* Should fail rather than stack overflow *)
module type S = sig
    type 'a t = 'a
      constraint 'a = < m : r >
    and r = (< m : r >) t
  end

module type T = S with type 'a t = 'b constraint 'a = < m : 'b >;;
[%%expect{|
module type S =
  sig type 'a t = 'a constraint 'a = < m : r > and r = < m : r > t end
Uncaught exception: Stack overflow

|}]

(* Correct *)
module type S = sig
    type t = Foo of r
    and r = t
  end

type s = Foo of s

module type T = S with type t = s
[%%expect{|
module type S = sig type t = Foo of r and r = t end
type s = Foo of s
module type T = sig type t = s = Foo of r and r = t end
|}]

(* Correct *)
module type S = sig
    type r = t
    and t = Foo of r
  end

type s = Foo of s

module type T = S with type t = s
[%%expect{|
module type S = sig type r = t and t = Foo of r end
type s = Foo of s
module type T = sig type r = t and t = s = Foo of r end
|}]

(* Should succeed *)
module type S = sig
    module rec M : sig
      type t = Foo of M.r
      type r = t
    end
  end

type s = Foo of s

module type T = S with type M.t = s
[%%expect{|
module type S = sig module rec M : sig type t = Foo of M.r type r = t end end
type s = Foo of s
Line 10, characters 23-35:
10 | module type T = S with type M.t = s
                            ^^^^^^^^^^^^
Error: This variant or record definition does not match that of type s
       Constructors do not match:
         Foo of s
       is not the same as:
         Foo of M.r
       The type s is not equal to the type M.r = M.t
|}]

(* Should succeed *)
module type S = sig
    module rec M : sig
      type t = private [`Foo of M.r]
      type r = t
    end
  end

type s = [`Foo of s]

module type T = S with type M.t = s
[%%expect{|
module type S =
  sig module rec M : sig type t = private [ `Foo of M.r ] type r = t end end
type s = [ `Foo of s ]
Line 10, characters 16-35:
10 | module type T = S with type M.t = s
                     ^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of M.t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = s
       is not included in
         type t = private [ `Foo of M.r ]
       The type s = [ `Foo of s ] is not equal to the type [ `Foo of M.r ]
       Type s = [ `Foo of s ] is not equal to type M.r = M.t
       Types for tag `Foo are incompatible
|}]

(* Should succeed *)
module type S = sig
  module rec M : sig
    module N : sig type t = private [`Foo of M.r] end
    type r = M.N.t
  end
end

module X = struct type t = [`Foo of t] end

module type T = S with module M.N = X
[%%expect{|
module type S =
  sig
    module rec M :
      sig
        module N : sig type t = private [ `Foo of M.r ] end
        type r = M.N.t
      end
  end
module X : sig type t = [ `Foo of t ] end
Line 10, characters 16-37:
10 | module type T = S with module M.N = X
                     ^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of M.N
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = [ `Foo of t ] end
       is not included in
         sig type t = private [ `Foo of M.r ] end
       Type declarations do not match:
         type t = [ `Foo of t ]
       is not included in
         type t = private [ `Foo of M.r ]
       The type [ `Foo of t ] is not equal to the type [ `Foo of M.r ]
       Type t = [ `Foo of t ] is not equal to type M.r = M.N.t
       Types for tag `Foo are incompatible
|}]

(* Should succeed *)
module type S = sig
    module rec M : sig
      module N : sig type t = M.r type s end
      type r = N.s
    end
  end

module X = struct type t type s = t end

module type T = S with module M.N = X
[%%expect{|
module type S =
  sig
    module rec M :
      sig module N : sig type t = M.r type s end type r = N.s end
  end
module X : sig type t type s = t end
Line 10, characters 16-37:
10 | module type T = S with module M.N = X
                     ^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of M.N
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = X.t type s = t end
       is not included in
         sig type t = M.r type s end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t = M.r
       The type X.t is not equal to the type M.r = M.N.s
|}]
