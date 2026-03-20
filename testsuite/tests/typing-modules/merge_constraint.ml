(* TEST
 ocamlrunparam = "l=1000000";
 expect;
*)

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
Error: Unbound type constructor "t"
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
            type 'a abstract = 'a abstract/2
            type 'a user = 'a user/2 = Foo of 'a abstract
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
            type 'a ind = 'a ind/2
            type t = t/2 = T : 'a ind -> t [@@unboxed]
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
      sig type 'a u = 'a list type +'a t = 'a t/2 constraint 'a = 'b u end
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
            type 'a user = 'a user/2 = Foo of 'a abstract
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
      (Header : Header) () ->
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
Error: This variant or record definition does not match that of type "s"
       Constructors do not match:
         "Foo of s"
       is not the same as:
         "Foo of M.r"
       The type "s" is not equal to the type "M.r" = "M.t"
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
Error: In this "with" constraint, the new definition of "M.t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = s
       is not included in
         type t = private [ `Foo of M.r ]
       The type "s" = "[ `Foo of s ]" is not equal to the type "[ `Foo of M.r ]"
       Type "s" = "[ `Foo of s ]" is not equal to type "M.r" = "M.t"
       Types for tag "`Foo" are incompatible
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
Error: In this "with" constraint, the new definition of "M.N"
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = [ `Foo of t ] end
       is not included in
         sig type t = private [ `Foo of M.r ] end
       Type declarations do not match:
         type t = [ `Foo of t ]
       is not included in
         type t = private [ `Foo of M.r ]
       The type "[ `Foo of t ]" is not equal to the type "[ `Foo of M.r ]"
       Type "t" = "[ `Foo of t ]" is not equal to type "M.r" = "M.N.t"
       Types for tag "`Foo" are incompatible
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
Error: In this "with" constraint, the new definition of "M.N"
       does not match its original definition in the constrained signature:
       Modules do not match:
         sig type t = X.t type s = t end
       is not included in
         sig type t = M.r type s end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t = M.r
       The type "X.t" is not equal to the type "M.r" = "M.N.s"
|}]

(* Module constraints with non-aliasable paths

   The first set of example use paths with functor applications to tests deep
   substitutions and handling of prefixes. The tests for functor parameters and
   recursive modules are below.
*)

(* Introduction of an invalid alias via a destructive module constraint should
   fail *)
module X = struct end
module F (_:sig end) = struct module X = struct end end
module type T = sig
  module X0 : sig end
  module X1 = X0
end with module X0 := F(X)
[%%expect{|
module X : sig end
module F : sig end -> sig module X : sig end end
Lines 3-6, characters 16-26:
3 | ................sig
4 |   module X0 : sig end
5 |   module X1 = X0
6 | end with module X0 := F(X)
Error: In this "with" constraint, replacing "X0" by "F(X)" would
       introduce an invalid alias at "X1"
|}]

(* Introduction of an invalid alias via a destructive module constraint should
   fail, also when the alias is made on a submodule *)
module X = struct end
module F (_:sig end) = struct module X = struct end end
module type T = sig
  module X0 : sig module X : sig end end
  module X1 = X0.X
end with module X0 := F(X)
[%%expect{|
module X : sig end
module F : sig end -> sig module X : sig end end
Lines 3-6, characters 16-26:
3 | ................sig
4 |   module X0 : sig module X : sig end end
5 |   module X1 = X0.X
6 | end with module X0 := F(X)
Error: In this "with" constraint, replacing "X0.X" by "F(X)" would
       introduce an invalid alias at "X1"
|}]

(* Introduction of an invalid alias via a deep destructive module constraint
   should fail when the enclosing module is aliased (it should also fail if the
   alias is not invalid: any destruction on an aliased module is disallowed.) *)
module X = struct end
module F (_:sig end) = struct module X = struct end end
module type T = sig
  module X0 : sig module X : sig end end
  module X1 = X0
end with module X0.X := F(X)
[%%expect{|
module X : sig end
module F : sig end -> sig module X : sig end end
Lines 3-6, characters 16-28:
3 | ................sig
4 |   module X0 : sig module X : sig end end
5 |   module X1 = X0
6 | end with module X0.X := F(X)
Error: This "with" constraint on "X0.X" changes "X0", which is aliased
       in the constrained signature (as "X1").
|}]

(* Introduction of an invalid alias via a deep destructive module constraint
   should fail, even when the alias is made on a submodule *)
module X = struct end
module F (_:sig end) = struct module X = struct end end
module type T = sig
  module X0 : sig module X : sig end end
  module X1 = X0.X
end with module X0.X := F(X)
[%%expect{|
module X : sig end
module F : sig end -> sig module X : sig end end
Lines 3-6, characters 16-28:
3 | ................sig
4 |   module X0 : sig module X : sig end end
5 |   module X1 = X0.X
6 | end with module X0.X := F(X)
Error: In this "with" constraint, replacing "X0.X" by "F(X)" would
       introduce an invalid alias at "X1"
|}]

(* Introduction of an invalid alias via a deep destructive module constraint
   should fail for functor arguments *)
module F (Y:sig end) = struct
  module type T = sig
    module X0 : sig end
    module X1 = X0
  end with module X0 := Y
end
[%%expect{|
Lines 2-5, characters 18-25:
2 | ..................sig
3 |     module X0 : sig end
4 |     module X1 = X0
5 |   end with module X0 := Y
Error: In this "with" constraint, replacing "X0" by "Y" would
       introduce an invalid alias at "X1"
|}]

(* Introduction of an invalid alias via a deep destructive module constraint
   should fail for recursive modules (inside the recursive knot) *)
module rec Xrec : sig end = struct
  module type T = sig
    module X0 : sig end
    module X1 = X0
  end with module X0 := Xrec
end
[%%expect{|
Lines 2-5, characters 18-28:
2 | ..................sig
3 |     module X0 : sig end
4 |     module X1 = X0
5 |   end with module X0 := Xrec
Error: In this "with" constraint, replacing "X0" by "Xrec" would
       introduce an invalid alias at "X1"
|}]

(* Conversely, all those example should succeed for valid aliases (and not
   destructive substitution inside an aliased module)*)
module Valid = struct module X = struct end end
module type T1 = sig
  module X0 : sig end
  module X1 = X0
end with module X0 := Valid
module type T2 = sig
  module X0 : sig module X : sig end end
  module X1 = X0.X
end with module X0 := Valid
module type T3 = sig
  module X0 : sig module X : sig end end
  module X1 = X0.X
end with module X0.X := Valid
[%%expect{|
module Valid : sig module X : sig end end
module type T1 = sig module X1 = Valid end
module type T2 = sig module X1 = Valid.X end
module type T3 = sig module X0 : sig end module X1 = Valid end
|}]

(* Invalid aliases should be caught early *)
module X = struct end
module F (_:sig end) = struct type t end
module M = struct
  module type Should_fail_here = (sig
    module X0 : sig end
    module X1 = X0
  end)
    with module X0 := F(X)

  module Should_not_fail_here : sig type t = int end =
  struct type t = bool end
end
[%%expect{|
module X : sig end
module F : sig end -> sig type t end
Lines 4-8, characters 33-26:
4 | .................................(sig
5 |     module X0 : sig end
6 |     module X1 = X0
7 |   end)
8 |     with module X0 := F(X)
Error: In this "with" constraint, replacing "X0" by "F(X)" would
       introduce an invalid alias at "X1"
|}]

(* Invalid aliases should be caught early (bis) *)
module X = struct end
module F (_:sig end) = struct type t end
module ShoudFail : sig end = struct
  module type T = (sig
    module X0 : sig end
    module X1 = X0
  end)
    with module X0 := F(X)
end
[%%expect{|
module X : sig end
module F : sig end -> sig type t end
Lines 4-8, characters 18-26:
4 | ..................(sig
5 |     module X0 : sig end
6 |     module X1 = X0
7 |   end)
8 |     with module X0 := F(X)
Error: In this "with" constraint, replacing "X0" by "F(X)" would
       introduce an invalid alias at "X1"
|}]

(*** [with type] with type constraints ***)

(* This first test is a regression test for #14117. *)
module type Non_destructive_with_type_with_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type 'a t2 constraint 'a = 'l * 'r
  module type S2 = S with type ('a, _) t = 'a t2
end
[%%expect{|
module type Non_destructive_with_type_with_constraint =
  sig
    module type S = sig type ('a, 'b) t constraint 'a = 'l * 'r end
    type 'a t2 constraint 'a = 'l * 'r
    module type S2 = sig type ('a, _) t = 'a t2 constraint 'a = 'b * 'c end
  end
|}]

module type Non_destructive_with_type_alias_with_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type ('a, 'b) t2 constraint 'a = 'l * 'r
  module type S2 = S with type ('a, 'b) t = ('a, 'b) t2
end
[%%expect{|
module type Non_destructive_with_type_alias_with_constraint =
  sig
    module type S = sig type ('a, 'b) t constraint 'a = 'l * 'r end
    type ('a, 'b) t2 constraint 'a = 'l * 'r
    module type S2 =
      sig type ('a, 'b) t = ('a, 'b) t2 constraint 'a = 'c * 'd end
  end
|}]

module type Non_destructive_with_type_alias_with_inconsistent_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type ('a, 'b) t2 constraint 'a = 'l * 'r * 's
  module type S2 = S with type ('a, 'b) t = ('a, 'b) t2
end
[%%expect{|
Line 6, characters 32-34:
6 |   module type S2 = S with type ('a, 'b) t = ('a, 'b) t2
                                    ^^
Error: The type constraints are not consistent.
       Type "'a * 'b * 'c" is not compatible with type "'d * 'e"
|}]

module type Destructive_with_type_with_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type 'a t2 constraint 'a = 'l * 'r
  module type S2 = S with type ('a, _) t := 'a t2
end
[%%expect{|
Line 6, characters 19-49:
6 |   module type S2 = S with type ('a, _) t := 'a t2
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Destructive substitutions are not supported for constrained
       types (other than when replacing a type constructor with
       a type constructor with the same arguments).
|}]

module type Destructive_with_type_alias_with_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type ('a, 'b) t2 constraint 'a = 'l * 'r
  module type S2 = S with type ('a, 'b) t := ('a, 'b) t2
end
[%%expect{|
module type Destructive_with_type_alias_with_constraint =
  sig
    module type S = sig type ('a, 'b) t constraint 'a = 'l * 'r end
    type ('a, 'b) t2 constraint 'a = 'l * 'r
    module type S2 = sig end
  end
|}]

module type Destructive_with_type_alias_with_inconsistent_constraint = sig
  module type S = sig
    type ('a, 'b) t constraint 'a = 'l * 'r
  end
  type ('a, 'b) t2 constraint 'a = 'l * 'r * 's
  module type S2 = S with type ('a, 'b) t := ('a, 'b) t2
end
[%%expect{|
Line 6, characters 32-34:
6 |   module type S2 = S with type ('a, 'b) t := ('a, 'b) t2
                                    ^^
Error: The type constraints are not consistent.
       Type "'a * 'b * 'c" is not compatible with type "'d * 'e"
|}]

(** Merging and approximation ***)
(* The test are made inside module types, as they only focus on approximation
   and typechecking of signatures of recursive modules, hence the
   implementations are irrelevant. The tests for module type constraints can be
   found in [module_type_substitution.ml] *)

(** Module constraints during approximation **)

(*  Merging modules constraints during the approximation phase of typechecking
    recursive modules should result in the right approximated signature, with
    the right set of visible fields. Concrete constraints (where a module that
    is already an alias is replaced by an equivalent alias) are not tested as it
    is unsupported (issue #13897) *)

(* Approximating a signature with a module constraint should merge the module
   (abstract, shallow, non destructive case) *)
module type Module_Abstract_Shallow = sig
  module X0 : sig type t end
  module rec X : (sig module X1 : sig end end with module X1 = X0)
     and Y : sig type u = X.X1.t end
end
[%%expect {|
module type Module_Abstract_Shallow =
  sig
    module X0 : sig type t end
    module rec X : sig module X1 : sig type t = X0.t end end
    and Y : sig type u = X.X1.t end
  end
|}]

(* Approximating a signature with a module constraint should merge the module
   (abstract, shallow, destructive case). We use shadowing to test that the
   destructed item has indeed been removed *)
module type Module_Abstract_Shallow_Destructive = sig
  module X0 : sig end
  module type S = sig module X1 : sig end end
  module rec X : (sig
                   module X1 : sig type t end
                   include (S with module X1 := X0)
                 end)
     and Y : sig type u = X.X1.t end
end
[%%expect {|
module type Module_Abstract_Shallow_Destructive =
  sig
    module X0 : sig end
    module type S = sig module X1 : sig end end
    module rec X : sig module X1 : sig type t end end
    and Y : sig type u = X.X1.t end
  end
|}]


(* Approximating a signature with a module constraint should merge the module
   (abstract, deep, non destructive case) *)
module type Module_Abstract_Deep = sig
  module X0 : sig type t end
  module rec X : (sig
                   module X1 : sig module X2 : sig end end
                 end with module X1.X2 = X0)
     and Y : sig type u = X.X1.X2.t end
end
[%%expect {|
module type Module_Abstract_Deep =
  sig
    module X0 : sig type t end
    module rec X :
      sig module X1 : sig module X2 : sig type t = X0.t end end end
    and Y : sig type u = X.X1.X2.t end
  end
|}]

(* There is no test for (abstract, deep, destructive case). The shallow test
   cannot be adapted, as the root module (X1) would still be shadowed *)

(* Ill-formed module constraints (when the field does not exist) should be
   caught during approximation.  *)
module type IllFormed_Module_No_Field = sig
  module X0 : sig end
  module rec X : (sig
                   module X1 : sig end

                   (* invalid, but the error should not be here *)
                   type 'a t
                   type u = (int, bool) t
                 end) with module X2 = X0
end
[%%expect {|
Line 9, characters 39-41:
9 |                  end) with module X2 = X0
                                           ^^
Error: The signature constrained by "with" has no component named "X2"
|}]


(* Ill-formed module constraints (when the field does not exist) should be
   caught during approximation (destructive case).  *)
module type IllFormed_Module_No_Field_Destructive = sig
  module X0 : sig end
  module rec X : (sig
                   module X1 : sig module X1' : sig end end

                   (* invalid, but the error should not be here *)
                   type 'a t
                   type u = (int, bool) t
                 end) with module X1.X2 = X0
end
[%%expect {|
Line 9, characters 42-44:
9 |                  end) with module X1.X2 = X0
                                              ^^
Error: The signature constrained by "with" has no component named "X1.X2"
|}]

(* Other forms of ill-formed module constraints (when the field exists) should
   be caught after approximation.  *)
module type IllFormed_Module_Other = sig
  module X0 : sig end
  module rec X : (sig
                   module X1 : sig type t end
                 end) with module X1 = X0
end
[%%expect {|
Lines 3-5, characters 17-41:
3 | .................(sig
4 |                    module X1 : sig type t end
5 |                  end) with module X1 = X0
Error: In this "with" constraint, the new definition of "X1"
       does not match its original definition in the constrained signature:
       Modules do not match: sig end is not included in sig type t end
       The type "t" is required but not provided
|}]

(** Type constraints during approximation **)

(*  Merging type constraints during the approximation phase of typechecking
    recursive modules should result in the right approximated signature, with
    the right set of visible fields. There is not much to check for non
    destructive cases, as approximated constraints are abstract anyway *)

(* Approximation a signature with a type constraint should merge the type
   (abstract, shallow, destructive case). We use shadowing to test that the
   type has indeed been removed *)
module type Type_Abstract_Shallow_Destructive = sig
  module rec X : (sig
                   type 'a t
                   include (sig type t end) with type t := int
                 end)
     and Y : sig type u = int X.t end
end
[%%expect {|
module type Type_Abstract_Shallow_Destructive =
  sig module rec X : sig type 'a t end and Y : sig type u = int X.t end end
|}]


(* Ill-formed type constraints (when the field does not exist) should be
   caught during approximation.  *)
module type IllFormed_Type_No_Field = sig
  module rec X : (sig
                   type t
                   (* invalid, but the error should not be here *)
                   type u = int t
                 end) with type v = int
end
[%%expect {|
Line 6, characters 27-39:
6 |                  end) with type v = int
                               ^^^^^^^^^^^^
Error: The signature constrained by "with" has no component named "v"
|}]


(* Ill-formed type constraints (when the field does not exist) should be
   caught during approximation (destructive case).  *)
module type IllFormed_Type_No_Field_Destructive = sig
  module rec X : (sig
                   type t
                   (* invalid, but the error should not be here *)
                   type u = int t
                 end) with type v := int
end
[%%expect {|
Line 6, characters 27-40:
6 |                  end) with type v := int
                               ^^^^^^^^^^^^^
Error: The signature constrained by "with" has no component named "v"
|}]

(* Other forms of ill-formed type constraints (when the field exists) should
   be caught after approximation.  *)
module type IllFormed_Module_Other = sig
  module rec X : (sig
                   type 'a t
                 end) with type t = int
end
[%%expect {|
Lines 2-4, characters 17-39:
2 | .................(sig
3 |                    type 'a t
4 |                  end) with type t = int
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = int
       is not included in
         type 'a t
       They have different arities.
|}]

(** Type constraints on module types inside recursive signatures should be
    properly typed. Here, if the [type t := int] constraint is ignored, the
    functor application becomes invalid. As paths with functor application can
    appear in signatures, it is crucial that the approximation phase get [T]
    right, it would be too late in the typechecking phase *)
module Empty = struct end
module rec X: sig
  module type T = sig type t end with type t := int
  module F(A:T): sig type t end
end = struct
  module type T = sig end
  module F(A:T) = struct type t end
end
and Y: sig type t = X.F(Empty).t end = struct
   type t = X.F(Empty).t
end

[%%expect{|
module Empty : sig end
module rec X :
  sig module type T = sig end module F : (A : T) -> sig type t end end
and Y : sig type t = X.F(Empty).t end
|}]


(** Type constraints and ghost items **)

(* Ghosts types (introduced by class definitions) should not be affected by type
   constraints (normal mode) *)
module type NoGhostTypeConstraintNormal = sig
    class v : object end
  end with type v = int
[%%expect{|
Lines 5-7, characters 42-23:
5 | ..........................................sig
6 |     class v : object end
7 |   end with type v = int
Error: The signature constrained by "with" has no component named "v"
|}]

(* Ghosts types (introduced by class definitions) should not be affected by type
   constraints (approx mode) *)
module type NoGhostTypeConstraintApprox = sig
         module rec X : sig class v : object end
                        end with type v = int
       end
[%%expect{|
Line 3, characters 33-45:
3 |                         end with type v = int
                                     ^^^^^^^^^^^^
Error: The signature constrained by "with" has no component named "v"
|}]
