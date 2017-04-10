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
val emit_string: string -> unit
val emit_int: int -> unit
val emit_targetint: Targetint.t -> unit
val emit_int32: int32 -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
val emit_char: char -> unit

val emit_directive : Asm_directives.Directive.t -> unit

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

(** Whether optimising for speed. *)
val fastcode_flag : bool ref

(** Linkage name of the function currently being compiled. *)
val function_name : Linkage_name.t ref

(** The entry point to the current function for a self-tail-recursive call. *)
val tailrec_entry_point : Cmm.label ref

(** Record the use of a symbol. *)
val add_used_symbol : Linkage_name.t -> unit

(** Symbols defined by the current compilation unit. *)
val symbols_defined : unit -> Linkage_name.Set.t

(** Symbols used by the current compilation unit. *)
val symbols_used : unit -> Linkage_name.Set.t

(** Total number of words occupied by constant literals not yet emitted. *)
val size_constants : int ref

(** Recording of calls to the GC. *)
val record_call_gc_site
   : label:Cmm.label
  -> return_label:Cmm.label
  -> frame_label:Cmm.label
  -> stack_offset:int
  -> unit

(** Recording of calls to [caml_ml_array_bound_error]. *)
val bound_error_label
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> Debuginfo.t
  -> stack_offset:int
  -> Cmm.label

(** Label a floating-point constant. *)
val float_constant : Int64.t -> Cmm.label

val num_float_constants : unit -> int

(** Label an integer constant. *)
val int_constant : Targetint.t -> Cmm.label

(** Force emission of pending constant literals immediately. *)
val emit_constants : unit -> unit

val begin_assembly : unit -> unit

val fundecl
   : ?branch_relaxation:((module Branch_relaxation.S) * int)
  -> Linearize.fundecl
  -> prepare:(Linearize.fundecl -> unit)
  -> emit_all:(fun_body:Linearize.instruction -> int)
  -> alignment_in_bytes:int
  -> emit_call:(Linkage_name.t -> unit)
  -> emit_jump_to_label:(Cmm.label -> unit)
  -> spacetime_before_uninstrumented_call:(Cmm.label -> unit)
  -> emit_numeric_constants:bool
  -> unit

val data : Cmm.data_item list -> unit

val end_assembly : emit_numeric_constants:bool -> unit
