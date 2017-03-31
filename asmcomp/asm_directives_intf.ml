(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emission of assembler directives that are supported on multiple targets. *)

module type S = sig
  (** Widths of data types. *)
  type width =
    | Thirty_two
    | Sixty_four

  (** Sections that hold for DWARF debugging information. *)
  type dwarf_section =
    | Debug_info
    | Debug_abbrev
    | Debug_aranges
    | Debug_loc
    | Debug_str
    | Debug_line

  (** The linker may share constants in [Eight_byte_literals] and
      [Sixteen_byte_literals] sections. *)
  type section =
    | Text
    | Data
    | Eight_byte_literals
    | Sixteen_byte_literals
    | Jump_tables
    | Dwarf of dwarf_section

  (** Retrieve the label that [switch_to_section] (below) will put at the start
      of the given section.  This function may be called before
      [switch_to_section] for the section concerned. *)
  val label_for_section : section -> Linearize.label

  (** Emit subsequent directives to the given section.  If this function
      has not been called before on the particular section, a label
      declaration will be emitted after declaring the section.
      Such labels may seem strange, but they are necessary so that
      references (e.g. DW_FORM_ref_addr / DW_FORM_sec_offset when emitting
      DWARF) to places that are currently at the start of these sections
      get relocated correctly when those places become not at the start
      (e.g. during linking). *)
  val switch_to_section : section -> unit

  (** Emit an 8-bit integer.  There is no padding or sign extension. *)
  val int8 : Numbers.Int8.t -> unit

  (** Emit a 16-bit integer.  There is no padding or sign extension. *)
  val int16 : Numbers.Int16.t -> unit

  (** Emit a 32-bit integer.  There is no padding or sign extension. *)
  val int32 : Int32.t -> unit

  (** Emit a 64-bit integer. *)
  val int64 : Int64.t -> unit

  (** Emit an integer whose width is that of an address on the target
      machine. *)
  val target_address : Targetint.t -> unit

  (** Emit a 64-bit integer in unsigned LEB128 variable-length encoding
      (cf. DWARF debugging information standard). *)
  val uleb128 : Int64.t -> unit

  (** Emit a 64-bit integer in signed LEB128 variable-length encoding. *)
  val sleb128 : Int64.t -> unit

  (** Emit a string (directly into the current section).  This function
      does not write a terminating null. *)
  val string : string -> unit

  (** Cache a string for later emission.  The returned label may be used to
      obtain the address of the string in the section.  This function does
      not emit anything.  (See [emit_cached_strings], below.)
      If a string is supplied to this function that is already in the cache
      then the previously-assigned label is returned, not a new one. *)
  val cache_string : string -> Linearize.label

  (** Emit the sequence of:
        label definition:
          <string><null terminator>
      pairs as per previous calls to [cache_string].  This function clears
      the cache. *)
  val emit_cached_strings : unit -> unit

  (** Emit a comment. *)
  val comment : string -> unit

  (** Assign a file number to a filename. *)
  val file : file_num:int -> file_name:string -> unit

  (** Mark the source location of the current assembly position. *)
  val loc : file_num:int -> line:int -> col:int -> unit

  (** Adjust the current frame address offset by the given number of bytes. *)
  val cfi_adjust_cfa_offset : bytes:int -> unit

  (** Mark the beginning of a function, for CFI purposes. *)
  val cfi_startproc : unit -> unit

  (** Mark the end of a function, for CFI purposes. *)
  val cfi_endproc : unit -> unit

  (** Mark that the call stack is not to be executable at runtime.  Not
      supported on all platforms. *)
  val mark_stack_non_executable : unit -> unit

  (** Leave as much space as is required to achieve the given alignment. *)
  val align : bytes:int -> unit

  (** Emit a directive giving the displacement between the given symbol and
      the current position. *)
  val size : string -> unit

  (** Leave a gap in the object file. *)
  val space : bytes:int -> unit

  (** Define a symbol at the current output position.  When emitting for MASM
      into a non-text section this will cause loads and stores to/from the
      symbol to be treated as if they are loading machine-width words (unless
      the instruction has an explicit width suffix). *)
  val define_symbol : Symbol.t -> unit

  (** Like [define_symbol], but when not using the middle-end type. *)
  val define_symbol' : string -> unit

  (** Define a function symbol.  An exception will be raised if the current
      section is not a text section. *)
  val define_function_symbol' : string -> unit

  (** Mark a symbol as global. *)
  val global : string -> unit

  (** Emit a machine-width reference to the given symbol. *)
  val symbol : Symbol.t -> unit

  (** Like [symbol], but when not using the middle-end type. *)
  val symbol' : string -> unit

  (** Mark a symbol as "private extern" (see assembler documentation for
      details). *)
  val private_extern : string -> unit

  (** Marker inside the definition of a lazy symbol stub (see platform or
      assembler documentation for details). *)
  val indirect_symbol : string -> unit

  (** Define a label at the current position in the current section.
      The treatment for MASM when emitting into non-text sections is as for
      [define_symbol], above. *)
  val define_label : Linearize.label -> unit

  (** Emit a machine-width reference to the given label. *)
  val label : Linearize.label -> unit

  (** Emit a machine-width reference to the address formed by adding the
      given byte offset to the address of the given symbol. *)
  val symbol_plus_offset
     : Symbol.t
    -> offset_in_bytes:Targetint.t
    -> unit

  (** The following functions calculate distances between various entities
      such as labels and symbols.  These distances are calculated at link time
      after the entities concerned have been relocated.  This means that they
      can be safely used even when the linker may insert (or remove) code or
      data between the points being measured. *)
  (* CR mshinwell: Let's verify that the relocation stuff is as expected *)

  (** Emit a machine-width reference giving the displacement between two given
      labels.  To obtain a positive result the symbol at the lower address
      should be the second argument, as for normal subtraction. *)
  val between_symbols : upper:Symbol.t -> lower:Symbol.t -> unit

  (** Like [between_symbols], but for two labels, emitting a 32-bit-wide
      reference.  The behaviour upon overflow is unspecified. *)
  val between_labels_32bit : upper:Cmm.label -> lower:Cmm.label -> unit

  (** Emit a machine-width reference giving the displacement between the
      lower symbol and the sum of the address of the upper label plus
      [offset_upper]. *)
  val between_symbol_and_label_offset
     : upper:Linearize.label
    -> lower:Symbol.t
    -> offset_upper:Targetint.t
    -> unit

  (** Emit a 32-bit-wide reference giving the displacement between obtained
      by subtracting the current assembly location from the sum of the address
      of the given label plus the given offset. *)
  (* CR mshinwell: Make sure that emit_label lines up with what Asm_directives
     does for int -> string label conversion *)
  val between_this_and_label_offset_32bit
     : upper:Linearize.label
    -> offset_upper:Targetint.t
    -> unit

  (** Emit an integer giving the distance obtained by subtracting the
      address of [base] from the address of [label].  [width] specifies the
      size of the integer. *)
  val offset_into_section_label
     : section:section
    -> label:Linearize.label
    -> width:width
    -> unit

  (** As for [offset_into_section_label], but using a symbol instead of
      a label as one end of the measurement. *)
  val offset_into_section_symbol
     : section:section
    -> symbol:Symbol.t
    -> width:width
    -> unit
end
