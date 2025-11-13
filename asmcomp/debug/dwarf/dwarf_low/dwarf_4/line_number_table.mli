(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** DWARF 4 line number table.

    Manages the line number program state machine and generates
    the .debug_line section. *)

(** A source position *)
type position = {
  file: string;
  line: int;
  column: int;
}

(** A line number program entry *)
type entry = {
  address: Code_address.t;
  position: position;
  is_stmt: bool;
  basic_block: bool;
  prologue_end: bool;
  epilogue_begin: bool;
}

(** Line number table *)
type t

(** Create a new line number table *)
val create : unit -> t

(** Add a line number entry *)
val add_entry : t -> entry -> unit

(** Mark the end of a sequence *)
val end_sequence : t -> unit

(** Get all file names referenced in the table *)
val files : t -> string list

(** Get the compilation directory *)
val set_comp_dir : t -> string -> unit

(** Emit the .debug_line section with address relocations.
    Returns (bytes, relocations) where relocations point to label addresses
    that need to be resolved by the assembler/linker. *)
val emit : t -> bytes * (int * string) list  (* (offset, label) pairs *)

(** Pretty-printer *)
val print : Format.formatter -> t -> unit
