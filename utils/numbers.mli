(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Modules about numbers, some of which satisfy {!Identifiable.S}.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

module Int : sig
  include Identifiable.S with type t = int

  (** [zero_to_n n] is the set of numbers \{0, ..., n\} (inclusive). *)
  val zero_to_n : int -> Set.t

  module Pair : Identifiable.S with type t = int * int
  module Triple : Identifiable.S with type t = int * int * int
end

module Int8 : sig
  type t

  val print : Format.formatter -> t -> unit

  val zero : t
  val one : t

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Int16 : sig
  type t

  val print : Format.formatter -> t -> unit

  val of_int_exn : int -> t
  val of_int64_exn : Int64.t -> t

  val to_int : t -> int
end

(** Do not use polymorphic comparison on the unsigned integer types. *)

module Uint8 : sig
  type t

  val print : Format.formatter -> t -> unit

  val zero : t
  val one : t

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Uint16 : sig
  type t

  val print : Format.formatter -> t -> unit

  val of_int_exn : int -> t
  val of_int64_exn : Int64.t -> t

  val to_int : t -> int
end

module Uint32 : sig
  type t

  val print : Format.formatter -> t -> unit

  val zero : t

  val of_int_exn : int -> t
  val of_int32 : Int32.t -> t
  val of_int64_exn : Int64.t -> t

  val to_int64 : t -> Int64.t
end

module Uint64 : sig
  type t

  val zero : t

  val succ : t -> t

  val of_int_exn : int -> t

  val of_uint8 : Uint8.t -> t
  val of_uint16 : Uint16.t -> t
  val of_uint32 : Uint32.t -> t

  val of_int32_exn : Int32.t -> t
  val of_int64_exn : Int64.t -> t

  val to_int64 : t -> Int64.t

  include Identifiable.S with type t := t
end

module Float : Identifiable.S with type t = float
