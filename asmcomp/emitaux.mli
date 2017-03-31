(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

val output_channel: out_channel ref

val reset : unit -> unit
val reset_debug_info: unit -> unit
val emit_debug_info: Debuginfo.t -> unit

val record_frame_descr :
  label:int ->              (* Return address *)
  frame_size:int ->         (* Size of stack frame *)
  live_offset:int list ->   (* Offsets/regs of live addresses *)
  raise_frame:bool ->       (* Is frame for a raise? *)
  Debuginfo.t ->            (* Location, if any *)
  unit

val emit_frames: unit -> unit

val is_generic_function: string -> bool

val binary_backend_available: bool ref
    (** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)

val create_asm_file: bool ref
    (** Are we actually generating the textual assembly file? *)
