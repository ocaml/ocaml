(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Register availability sets. *)

type t =
  | Ok of Reg_with_debug_info.Set.t
  | Unreachable

val map : t -> f:(Reg_with_debug_info.Set.t -> Reg_with_debug_info.Set.t) -> t

(** Intersection of availabilities. *)
val inter : t -> t -> t

(** Return a subset of the given availability set which contains no registers
    that are not associated with debug info; and where no two registers
    share the same location. *)
val canonicalise : t -> t

(** Like [Reg_with_debug_info.made_unavailable_by_clobber]. *)
val made_unavailable_by_clobber
   : t
  -> regs_clobbered:Reg.t array
  -> register_class:(Reg.t -> int)
  -> t

val equal : t -> t -> bool

val subset : t -> t -> bool

val find_reg_opt : t -> Reg.t -> Reg_with_debug_info.t option

val find_all_holding_value_of
   : t
  -> Reg_with_debug_info.Holds_value_of.t
  -> Reg_with_debug_info.t list

val print
   : print_reg:(Format.formatter -> Reg.t -> unit)
  -> Format.formatter
  -> t
  -> unit
(** For debugging purposes only. *)
