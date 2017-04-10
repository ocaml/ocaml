(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Common functions for emitting assembly code *)

(** The channel for textual assembly emission. *)
val output_channel : out_channel ref

(** Emit a string to the output channel for textual assembly emission. *)
val emit_string : string -> unit

(** Emit an integer to the output channel for textual assembly emission. *)
val emit_int : int -> unit

(** Emit a character to the output channel for textual assembly emission. *)
val emit_char : char -> unit

(** Emit an integer, whose width is the natural width of the target
    machine, to the output channel for textual assembly emission. *)
val emit_targetint : Targetint.t -> unit

(** Emit a 32-bit integer to the output channel for textual assembly
    emission. *)
val emit_int32: int32 -> unit

(** Printing using format strings to the output channel for textual
    assembly emission. *)
val emit_printf : ('a, out_channel, unit) format -> 'a

(** Emit an assembly directive as text. *)
val emit_directive : Asm_directives.Directive.t -> unit

(** Emit location information into the assembly output for the given
    debug info. *)
val emit_debug_info: Debuginfo.t -> unit

(** Whether a binary backend is available.  If yes, we don't need to generate
    the textual assembly file (unless the user requests it with -S). *)
val binary_backend_available: bool ref

(** Are we actually generating the textual assembly file? *)
val create_asm_file: bool ref

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

(** Record a frame descriptor at the current position and return the label
    that points at it.
    In a backend it will be found convenient to specialise this function
    (and others below) to the particular target-specific [frame_size] and
    [slot_offset] functions. *)
val record_frame_label
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> live:Reg.Set.t
  -> raise_:bool
  -> Debuginfo.t
  -> Cmm.label

(** Record a frame descriptor at the current position. *)
val record_frame
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> live:Reg.Set.t
  -> raise_:bool
  -> Debuginfo.t
  -> unit

(** Record a call to the GC. *)
val record_call_gc_site
   : label:Cmm.label
  -> return_label:Cmm.label
  -> frame_label:Cmm.label
  -> stack_offset:int
  -> unit

(** Record a call to [caml_ml_array_bound_error]. *)
val bound_error_label
   : frame_size:(unit -> int)
  -> slot_offset:(Reg.stack_location -> int -> int)
  -> ?label:Cmm.label
  -> Debuginfo.t
  -> stack_offset:int
  -> Cmm.label

(** Store a floating point constant for later emission.  The returned label
    may be used to reference it. *)
val float_constant : Int64.t -> Cmm.label

(** How many floating point constants there are awaiting emission. *)
val num_float_constants : unit -> int

(** Store an integer constant for later emission.  The returned label
    may be used to reference it. *)
val int_constant : Targetint.t -> Cmm.label

(** Start assembly output for a compilation unit. *)
val begin_assembly : unit -> unit

(** Emit assembly for a function declaration. *)
val fundecl
   : ?branch_relaxation:((module Branch_relaxation.S) * int)
  -> Linearize.fundecl
  -> prepare:(Linearize.fundecl -> unit)
  -> emit_all:(fun_body:Linearize.instruction -> int)
  -> alignment_in_bytes:int
     (** The desired alignment of the function entry point. *)
  -> emit_call:(Linkage_name.t -> unit)
  -> emit_jump_to_label:(Cmm.label -> unit)
  -> spacetime_before_uninstrumented_call:(Cmm.label -> unit)
  -> emit_numeric_constants:bool
     (** [true] if float and integer constants are to be emitted in the text
         section following the function's body. *)
  -> unit

(** Emit items of Cmm data into the data section. *)
val data : Cmm.data_item list -> unit

(** Force emission of pending constant literals immediately.

    This is only needed when the functionality provided below (via
    [emit_numeric_constants]) is insufficient for target-specific reasons,
    such as strict requirements on the placement of constants.

    If [in_current_section] is [false] then the constants will be emitted
    into the appropriate read-only data / shared constant sections.  After
    that the caller of this function must switch to the section in which they
    desire to continue emission. *)
val emit_constants : in_current_section:bool -> unit

(** Finish assembly output for a compilation unit.
    If [emit_numeric_constants] is [true] then float and integer constants
    will be emitted into the appropriate read-only data / read-only
    shared constant sections.  Otherwise they will not be emitted (typically
    used when [emit_numeric_constants] to [fundecl], above, was [true]). *)
val end_assembly : emit_numeric_constants:bool -> unit

(** Reset the emitter, to be used between compilation units. *)
val reset : unit -> unit
