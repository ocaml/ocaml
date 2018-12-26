(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstraction layer for the emission of assembly directives that conceals
    many intricate details differing between target systems. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-someday mshinwell: Use this module throughout the backends as per
   the original GPR. *)

(** Emit subsequent directives to the given section.  If this function
    has not been called before on the particular section, a label
    declaration will be emitted after declaring the section.
    Such labels may seem strange, but they are necessary so that
    references (e.g. DW_FORM_ref_addr / DW_FORM_sec_offset when emitting
    DWARF) to places that are currently at the start of these sections
    get relocated correctly when those places become not at the start
    (e.g. during linking). *)
val switch_to_section : Asm_section.t -> unit

(** Emit subsequent directives to the given section, where the section must
    not be one of those in type [section] (see above).  The section is
    specified by the three components of a typical assembler section-switching
    command.  This function is only intended to be used for target-specific
    sections. *)
val switch_to_section_raw
   : names:string list
  -> flags:string option
  -> args:string list
  -> unit

(** Abbreviation for [switch_to_section Text]. *)
val text : unit -> unit

(** Abbreviation for [switch_to_section Data]. *)
val data : unit -> unit

(** Emit an 8-bit signed integer.  There is no padding or sign extension.
    If the [comment] is specified it will be put on the same line as the
    integer. *)
val int8 : ?comment:string -> Numbers.Int8.t -> unit

(** Emit a 16-bit signed integer.  There is no padding or sign extension. *)
val int16 : ?comment:string -> Numbers.Int16.t -> unit

(** Emit a 32-bit signed integer.  There is no padding or sign extension. *)
val int32 : ?comment:string -> Int32.t -> unit

(** Emit a 64-bit signed integer. *)
val int64 : ?comment:string -> Int64.t -> unit

(** Emit an 8-bit unsigned integer.  There is no padding. *)
val uint8 : ?comment:string -> Numbers.Uint8.t -> unit

(** Emit an 16-bit unsigned integer.  There is no padding. *)
val uint16 : ?comment:string -> Numbers.Uint16.t -> unit

(** Emit an 32-bit unsigned integer.  There is no padding. *)
val uint32 : ?comment:string -> Numbers.Uint32.t -> unit

(** Emit an 64-bit unsigned integer.  There is no padding. *)
val uint64 : ?comment:string -> Numbers.Uint64.t -> unit

(** Emit a signed integer whose width is that of an address on the target
    machine.  There is no padding or sign extension. *)
(* CR-soon mshinwell: Target addresses should not be signed *)
val targetint : ?comment:string -> Targetint.t -> unit

(** Emit a 64-bit integer in unsigned LEB128 variable-length encoding
    (cf. DWARF debugging information standard). *)
val uleb128 : ?comment:string -> Numbers.Uint64.t -> unit

(** Emit a 64-bit integer in signed LEB128 variable-length encoding. *)
val sleb128 : ?comment:string -> Int64.t -> unit

(** Emit a 32-bit-wide floating point number. *)
val float32 : float -> unit

(** Emit a 64-bit-wide floating point number. *)
val float64 : float -> unit

(** Emit a 64-bit-wide floating point number whose bits are contained
    in an [Int64.t]. *)
val float64_from_bits : Int64.t -> unit

(** Emit a string (directly into the current section).  This function
    does not write a terminating null. *)
val string : ?comment:string -> string -> unit

(** Cache a string for later emission.  The returned label may be used to
    obtain the address of the string in the section.  This function does
    not emit anything.  (See [emit_cached_strings], below.)
    If a string is supplied to this function that is already in the cache
    then the previously-assigned label is returned, not a new one. *)
val cache_string : ?comment:string -> Asm_section.t -> string -> Asm_label.t

(** Emit the sequence of:
      label definition:
        <string><null terminator>
    pairs as per previous calls to [cache_string] with appropriate directives
    to switch section interspersed.  This function clears the cache. *)
val emit_cached_strings : unit -> unit

(** Emit a comment. *)
val comment : string -> unit

(** Assign a file number to a filename. *)
val file : file_num:int -> file_name:string -> unit

(** Mark the source location of the current assembly position. *)
val loc : file_num:int -> line:int -> col:int -> unit

(** Emit a blank line. *)
val new_line : unit -> unit

(** Adjust the current frame address offset by the given number of bytes.
    This and other CFI functions will not emit anything in the case where
    CFI is not supported on the target. *)
val cfi_adjust_cfa_offset : bytes:int -> unit

(** Define the current frame address offset.
    This and other CFI functions will not emit anything in the case where
    CFI is not supported on the target. *)
val cfi_def_cfa_offset : bytes:int -> unit

(** Note that the previous value of [reg] is saved at [offset] from
    the current frame address. *)
val cfi_offset : reg:int -> offset:int -> unit

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
    the current position.  This should only be used to state sizes of
    blocks (e.g. functions) emitted immediately prior into the assembly stream.
    [size_of] may be specified when the symbol used for measurement differs
    from that whose size is being stated (e.g. on POWER with ELF ABI v1). *)
val size : ?size_of:Asm_symbol.t -> Asm_symbol.t -> unit

(** Leave a gap in the object file. *)
val space : bytes:int -> unit

(** Define a data ("object") symbol at the current output position.  When
    emitting for MASM this will cause loads and stores to/from the symbol to
    be treated as if they are loading machine-width words (unless the
    instruction has an explicit width suffix). *)
val define_data_symbol : Asm_symbol.t -> unit

(** Define a function symbol at the current output position.  An exception
    will be raised if the current section is not a text section. *)
val define_function_symbol : Asm_symbol.t -> unit

(** Mark a symbol as global. *)
val global : Asm_symbol.t -> unit

(** Emit a machine-width reference to the given symbol. *)
val symbol : ?comment:string -> Asm_symbol.t -> unit

(** Mark a symbol as "private extern" (see assembler documentation for
    details). *)
val private_extern : Asm_symbol.t -> unit

(** Marker inside the definition of a lazy symbol stub (see platform or
    assembler documentation for details). *)
val indirect_symbol : Asm_symbol.t -> unit

(** Define a label at the current position in the current section.
    The treatment for MASM when emitting into non-text sections is as for
    [define_symbol], above. *)
val define_label : Asm_label.t -> unit

(** Emit a machine-width reference to the given label. *)
val label : ?comment:string -> Asm_label.t -> unit

(** Emit a machine-width reference to the address formed by adding the
    given byte offset to the address of the given symbol.  The symbol may be
    in a compilation unit and/or section different from the current one. *)
val symbol_plus_offset
   : Asm_symbol.t
  -> offset_in_bytes:Targetint.t
  -> unit

(** Emit a machine-width reference giving the displacement between two given
    symbols.  To obtain a positive result the symbol at the [lower] address
    should be the second argument, as for normal subtraction.  The symbols
    must be in the current compilation unit and in the same section. *)
val between_symbols_in_current_unit
   : upper:Asm_symbol.t
  -> lower:Asm_symbol.t
  -> unit

(** Like [between_symbols], but for two labels, emitting a 16-bit-wide
    reference.  The behaviour upon overflow is unspecified.  The labels must
    be in the same section. *)
val between_labels_16_bit
   : ?comment:string
  -> upper:Asm_label.t
  -> lower:Asm_label.t
  -> unit
  -> unit

(** Like [between_symbols], but for two labels, emitting a 32-bit-wide
    reference.  The behaviour upon overflow is unspecified.  The labels must
    be in the same section. *)
val between_labels_32_bit
   : ?comment:string
  -> upper:Asm_label.t
  -> lower:Asm_label.t
  -> unit
  -> unit

(** Like [between_symbols], but for two labels, emitting a 64-bit-wide
    reference.  The labels must be in the same section. *)
val between_labels_64_bit
   : ?comment:string
  -> upper:Asm_label.t
  -> lower:Asm_label.t
  -> unit
  -> unit

(** Emit a machine-width reference giving the displacement between the
    [lower] symbol and the sum of the address of the [upper] label plus
    [offset_upper].  The [lower] symbol must be in the current compilation
    unit.  The [upper] label must be in the same section as the [lower]
    symbol. *)
val between_symbol_in_current_unit_and_label_offset
   : ?comment:string
  -> upper:Asm_label.t
  -> lower:Asm_symbol.t
  -> offset_upper:Targetint.t
  -> unit

(** Emit a 32-bit-wide reference giving the displacement between obtained
    by subtracting the current assembly location from the sum of the address
    of the given label plus the given offset.  The label must be in the
    same section as the assembler is currently emitting into. *)
(* CR mshinwell: double-check use of this function *)
val between_this_and_label_offset_32bit
   : upper:Asm_label.t
  -> offset_upper:Targetint.t
  -> unit

(** Emit an offset into a DWARF section given a label identifying the place
    within such section. *)
val offset_into_dwarf_section_label
   : ?comment:string
  -> Asm_section.dwarf_section
  -> Asm_label.t
  -> width:Target_system.machine_width
  -> unit

(** Emit an offset into a DWARF section given a symbol identifying the place
    within such section.  The symbol may only be in a compilation unit different
    from the current one if the supplied section is [Debug_info].  The symbol
    must always be in the given section. *)
val offset_into_dwarf_section_symbol
   : ?comment:string
  -> Asm_section.dwarf_section
  -> Asm_symbol.t
  -> width:Target_system.machine_width
  -> unit

module Directive : sig
  module Constant : sig
    type t = private
      | Signed_int of Int64.t
      | Unsigned_int of Numbers.Uint64.t
      | This
      | Named_thing of string
      (** [Named_thing] covers symbols, labels and variables. (Name mangling
          conventions have by now been applied to these entities.) *)
      | Add of t * t
      | Sub of t * t
  end

  module Constant_with_width : sig
    (** A constant together with a width indicating the number of bytes in
        the object file within which the constant is to fit.  Some validation
        is performed on values of type [t] to try to ensure that this is the
        case, but it cannot be exhaustive, as the values of [This] and
        [Named_thing] constructions are not known. *)
    type t

    val constant : t -> Constant.t

    type width_in_bytes = private
      | Eight
      | Sixteen
      | Thirty_two
      | Sixty_four

    val width_in_bytes : t -> width_in_bytes
  end

  type thing_after_label = private
    | Code
    | Machine_width_data

  type comment = private string

  (** Internal representation of directives.  Only needed if writing a custom
      assembler or printer instead of using [print], below.
      Symbols that occur in values of type [t] are encoded as [string]s and
      have had all necessary prefixing, mangling, escaping and suffixing
      applied. *)
  type t = private
    | Align of { bytes : int; }
    | Bytes of { str : string; comment : string option; }
    | Cfi_adjust_cfa_offset of int
    | Cfi_def_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of { reg : int; offset : int; }
    | Cfi_startproc
    | Comment of comment
    | Const of { constant : Constant_with_width.t; comment : string option; }
    | Direct_assignment of string * Constant.t
    | File of { file_num : int option; filename : string; }
    | Global of string
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | New_label of string * thing_after_label
    | New_line
    | Private_extern of string
    | Section of {
        names : string list;
        flags : string option;
        args : string list;
      }
    | Size of string * Constant.t
    | Sleb128 of { constant : Constant.t; comment : string option; }
    | Space of { bytes : int; }
    | Type of string * string
    | Uleb128 of { constant : Constant.t; comment : string option; }

  (** Translate the given directive to textual form.  This produces output
      suitable for either gas or MASM as appropriate. *)
  val print : Buffer.t -> t -> unit
end

(** To be called by the emitter at the very start of code generation.
    [big_endian] should always be [Arch.big_endian].
    Calling the functions in this module will cause directives to be passed
    to the given [emit] function (a typical implementation of which will just
    call [Directive.print] on its parameter).
    This function switches to the text section. *)
val initialize
   : big_endian:bool
  -> emit:(Directive.t -> unit)
  -> unit

(** Reinitialize the emitter before compiling a different source file. *)
val reset : unit -> unit
