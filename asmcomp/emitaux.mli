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

val record_frame_label
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> live:Reg.Set.t
  -> raise_:bool
  -> Debuginfo.t
  -> Cmm.label

val record_frame
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> live:Reg.Set.t
  -> raise_:bool
  -> Debuginfo.t
  -> unit

val emit_frames: unit -> unit

val binary_backend_available: bool ref
    (** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)

val create_asm_file: bool ref
    (** Are we actually generating the textual assembly file? *)

(** Recording of calls to the GC. *)
val record_call_gc_site
   : label:Cmm.label
  -> return_label:Cmm.label
  -> frame_label:Cmm.label
  -> unit

(** Recording of calls to [caml_ml_array_bound_error]. *)
val bound_error_label
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> Debuginfo.t
  -> Cmm.label

(** Label a floating-point constant. *)
val float_constant : float -> Cmm.label

(** Label an integer constant. *)
val int_constant : nativeint -> Cmm.label

val begin_assembly : unit -> unit

val fundecl
   : ?branch_relaxation:((module Branch_relaxation.S) * int)
  -> Linearize.fundecl
  -> prepare:(Linearize.fundecl -> unit)
  -> emit_all:(fun_body:Linearize.instruction -> unit)
  -> alignment_in_bytes:int
  -> emit_call:(Linkage_name.t -> unit)
  -> emit_jump_to_label:(Cmm.label -> unit)
  -> spacetime_before_uninstrumented_call:(Cmm.label -> unit)
  -> emit_numeric_constants:bool
  -> unit

val data : Cmm.data_item list -> unit

val end_assembly : emit_numeric_constants:bool -> unit
