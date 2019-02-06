(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of labels in the assembly stream.

    We think as labels as a construct within the assembler's metalanguage.
    As such, we neither expect them to be accessible from outside the object
    file in which they are defined, nor to be available for examination in
    the object file itself (e.g. via objdump).  In other words, we do not
    treat them like "symbols".

    Labels are tied to sections.  This enables certain checks to be performed
    when constructions involving such labels are built (e.g. in
    [Asm_directives]).

    Labels' names must be unique within the assembly stream for a single
    compilation unit, including across sections.  (This criterion is not
    checked.)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** To be called by the emitter at the very start of code generation.
    [new_label] should always be [Cmm.new_label]. *)
val initialize
   : new_label:(unit -> int)
  -> unit

(** The type of labels. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Create a fresh integer-valued label (using the [new_label] function passed
    to [initialize], below). *)
val create : Asm_section.t -> t

(** Create an integer-valued label. *)
val create_int : Asm_section.t -> int -> t

(** Create a textual label.  The supplied name must not require escaping.
    The supplied name must not include any platform-specific prefix
    (e.g. "L", ".L", etc). *)
val create_string : Asm_section.t -> string -> t

(** Convert a label to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a label. *)
val encode : t -> string

(** Which section a label is in. *)
val section : t -> Asm_section.t

(** Retrieve a distinguished label that is suitable for identifying the start
    of the given section within a given compilation unit's assembly file. *)
val for_section : Asm_section.t -> t

(** Like [label], but for DWARF sections only. *)
val for_dwarf_section : Asm_section.dwarf_section -> t
