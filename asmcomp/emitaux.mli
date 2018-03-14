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
val emit_nativeint: nativeint -> unit
val emit_int32: int32 -> unit
val emit_symbol: char -> string -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
val emit_char: char -> unit
val emit_string_literal: string -> unit
val emit_string_directive: string -> string -> unit
val emit_bytes_directive: string -> string -> unit
val emit_float64_directive: string -> int64 -> unit
val emit_float64_split_directive: string -> int64 -> unit
val emit_float32_directive: string -> int32 -> unit

val reset : unit -> unit
val reset_debug_info: unit -> unit
val emit_debug_info: Debuginfo.t -> unit
val emit_debug_info_gen :
  Debuginfo.t ->
  (file_num:int -> file_name:string -> unit) ->
  (file_num:int -> line:int -> col:int -> unit) -> unit

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_raise: bool;                     (* Is frame for a raise? *)
    fd_debuginfo: Debuginfo.t }         (* Location, if any *)

val frame_descriptors : frame_descr list ref

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

val emit_frames: emit_frame_actions -> unit

val is_generic_function: string -> bool

val cfi_startproc : unit -> unit
val cfi_endproc : unit -> unit
val cfi_adjust_cfa_offset : int -> unit
val cfi_offset : reg:int -> offset:int -> unit
val cfi_def_cfa_offset : int -> unit
val cfi_remember_state : unit -> unit
val cfi_restore_state : unit -> unit

val binary_backend_available: bool ref
    (** Is a binary backend available.  If yes, we don't need
        to generate the textual assembly file (unless the user
        request it with -S). *)

val create_asm_file: bool ref
    (** Are we actually generating the textual assembly file? *)

val emit_block_header_for_closure
   : word_directive:string
  -> comment_string:string
  -> function_entry_points_are_doubleword_aligned:bool
  -> unit
