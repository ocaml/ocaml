(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Basic types corresponding to the compiler's target machine.
    We provide for the future potential of the compiler's target being
    selected at runtime. *)

module Address : sig
  (** Addresses on the target. *)

  type t = private
    | Int32 of Int32.t
    | Int64 of Int64.t

  val zero : unit -> t

  val all_ones : unit -> t

  type word_size = Four | Eight
  val word_size : unit -> word_size

  val of_int_exn : int -> t
  val to_int64 : t -> Int64.t
end
