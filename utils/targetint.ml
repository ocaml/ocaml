(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type repr =
  | Int32 of int32
  | Int64 of int64

module type S = sig
  type t
  val zero : t
  val one : t
  val minus_one : t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val max_int : t
  val min_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val of_int : int -> t
  val of_int_exn : int -> t
  val of_nativeint_exn : nativeint -> t
  val to_int : t -> int
  val of_float : float -> t
  val to_float : t -> float
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val repr: t -> repr
  val fits_in_16_bits : t -> bool
  val fits_in_32_bits : t -> bool
  val is_zero : t -> bool
  val strictly_negative : t -> bool
end

let size = Sys.word_size
(* Later, this will be set by the configure script
   in order to support cross-compilation. *)

module Int32 = struct
  include Int32
  let of_int_exn =
    match Sys.word_size with (* size of [int] *)
    | 32 ->
        Int32.of_int
    | 64 ->
        fun n ->
          if n < Int32.(to_int min_int) || n > Int32.(to_int max_int) then
            Misc.fatal_errorf "Targetint.of_int_exn: 0x%x out of range" n
          else
            Int32.of_int n
    | _ ->
        assert false
  let of_nativeint_exn = Nativeint.to_int32
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let repr x = Int32 x

  let fits_in_16_bits n =
    n >= -0x8000l && n <= 0x7FFFl

  let fits_in_32_bits n =
    n >= -0x8000_0000l && n <= 0x7FFF_FFFFl

  let is_zero n = (n = 0l)
  let strictly_negative n = (n < 0l)
end

module Int64 = struct
  include Int64
  let of_int_exn = Int64.of_int
  let of_nativeint_exn = Int64.of_nativeint
  let of_int64 x = x
  let to_int64 x = x
  let repr x = Int64 x

  let fits_in_16_bits n =
    n >= -0x8000L && n <= 0x7FFFL

  let fits_in_32_bits n =
    n >= -0x8000_0000L && n <= 0x7FFF_FFFFL

  let is_zero n = (n = 0L)
  let strictly_negative n = (n < 0L)
end

include (val
          (match size with
           | 32 -> (module Int32)
           | 64 -> (module Int64)
           | _ -> assert false
          ) : S)
