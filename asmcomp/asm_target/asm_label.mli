(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A label in a specific section within the assembly stream. They may be either
    numeric or textual. (Numeric ones are converted to textual ones by this
    module.) The argument to [String] should not include any platform-specific
    prefix (such as "L", ".L", etc).

    Label's numeric or textual names are unique within the assembly stream
    for a single compilation unit, even across sections.

    Note: Labels are not symbols in the usual sense---they are a construct
    in the assembler's metalanguage and not accessible in the object
    file---although on macOS the terminology for labels appears to be
    "assembler local symbols".
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create a fresh integer-valued label (using the [new_label] function passed
    to [initialize], below). *)
val create : Asm_section.t -> t

(** Create an integer-valued label. *)
val create_int : Asm_section.t -> int -> t

(** Create a textual label.  The supplied name must not require escaping. *)
val create_string : Asm_section.t -> string -> t

(** Convert a label to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a label. *)
val encode : t -> string

(** To be called by the emitter at the very start of code generation.
    [new_label] should always be [Cmm.new_label]. *)
val initialize
   : new_label:(unit -> int)
  -> unit

(** Which section a label is in. *)
val section : t -> Asm_section.t

include Identifiable.S with type t := t

(** Retrieve a distinguished label that is suitable for identifying the start
    of the given section within a given compilation unit's assembly file. *)
val for_section : Asm_section.t -> t

(** Like [label], but for DWARF sections only. *)
val for_dwarf_section : Asm_section.dwarf_section -> t
