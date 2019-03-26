(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functionality that permits intermediate representations such as Ir,
    Mach and Linear to be debugged. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Make_mark_functions (P : sig
  type t

  val position : t -> Debuginfo.Code_range.t option
end) : sig
  val mark_start_location : Format.formatter -> P.t -> unit
  val mark_end_location : Format.formatter -> P.t -> unit
end

module type S = sig
  type ir
  type t

  val create
     : ir_file:string
    -> ir_chan:Stdlib.out_channel
    -> t

  val ir_file : t -> string

  val write_ir_to_channel_and_fix_up_debuginfo : t -> ir -> ir
end

module Make (Ir : sig
  type t

  val print_without_debuginfo : Format.formatter -> t -> unit

  val rewrite_debuginfo
     : t
    -> rewrite_code_range:(Debuginfo.Code_range.t -> Debuginfo.Code_range.t)
    -> t
end) : S with type ir := Ir.t
