(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstraction over backends for the emission of assembler directives. *)

(* CR mshinwell: figure out build system nonsense to have either this or
   the textual one in place for any given target. *)

type width =
  | Thirty_two
  | Sixty_four

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type section =
  | Dwarf of dwarf_section

(** Retrieve the label that [switch_to_section] (below) will put at the start
    of the given section.  This function may be called before
    [switch_to_section] for the section concerned. *)
val label_for_section : section -> Linearize.label

module type S = sig
  (** To be called by the emitter at the very start of code generation. *)
  val init : unit -> unit

  (** Emit subsequent directives to the given section.  If this function
      has not been called before on the particular section, a label
      declaration will be emitted after declaring the section.
      Such labels may seem strange, but they are necessary so that
      references (e.g. DW_FORM_ref_addr / DW_FORM_sec_offset when emitting
      DWARF) to places that are currently at the start of these sections
      get relocated correctly when those places become not at the start
      (e.g. during linking). *)
  val switch_to_section : section -> unit

  (** Emit a machine-width reference to the given symbol. *)
  val symbol : Symbol.t -> unit

  (** Define a symbol at the current output position. *)
  val define_symbol : Symbol.t -> unit

  (** Emit a machine-width reference giving the displacement between the
      two given symbols.  To obtain a positive result the symbol at the
      lower address should be the second argument (just like subtraction). *)
  val between_symbols : upper:Symbol.t -> lower:Symbol.t -> unit

  (** Emit a machine-width reference giving the displacement between the
      lower symbol and the sum of the address of the upper label plus
      [offset_upper]. *)
  val between_symbol_and_label_offset
     : upper:Linearize.label
    -> lower:Symbol.t
    -> offset_upper:Target_system.Address.t
    -> unit

  (** Emit a machine-width reference to the address formed by adding the
      given byte offset to the address of the given symbol. *)
  val symbol_plus_offset
     : Symbol.t
    -> offset_in_bytes:Target_system.Address.t
    -> unit

  (** Emit a machine-width reference to the given label. *)
  val label : Linearize.label -> unit

  (** Define a label at the current position in the current section. *)
  val label_declaration : label_name:Linearize.label -> unit

  (** Emit an 8-bit integer (not padded). *)
  val int8 : Numbers.Int8.t -> unit

  (** Emit a 16-bit integer (not padded). *)
  val int16 : Numbers.Int16.t -> unit

  (** Emit a 32-bit integer (not padded). *)
  val int32 : Int32.t -> unit

  (** Emit a 64-bit integer. *)
  val int64 : Int64.t -> unit

  (** Emit an integer whose width is that of an address on the target
      machine. *)
  val target_address : Target_system.Address.t -> unit

  (** Emit a 64-bit integer in unsigned LEB128 variable-length encoding. *)
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

  (** Reinitialize the emitter before compiling a different source file. *)
  val reset : unit -> unit
end
