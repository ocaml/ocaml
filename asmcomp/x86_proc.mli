(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(** Definitions shared between the 32 and 64 bit Intel backends. *)

open X86_ast

(** Helpers for textual emitters *)

val string_of_reg8l: reg64 -> string
val string_of_reg8h: reg8h -> string
val string_of_reg16: reg64 -> string
val string_of_reg32: reg64 -> string
val string_of_reg64: reg64 -> string
val string_of_registerf: registerf -> string
val string_of_condition: condition -> string
val string_of_rounding: rounding -> string


(** Buffer of assembly code *)

val emit: instruction -> unit
val directive: asm_line -> unit
val reset_asm_code: unit -> unit

(** Code emission *)

val generate_code: (X86_ast.asm_line list -> unit) option -> unit
  (** Post-process the stream of instructions.  Dump it (using
      the provided syntax emitter) in a file (if provided) and
      compile it with an internal assembler (if registered
      through [register_internal_assembler]). *)

val assemble_file: (*infile*) string -> (*outfile*) string -> (*retcode*) int
(** Generate an object file corresponding to the last call to
    [generate_code].  An internal assembler is used if available (and
    the input file is ignored). Otherwise, the source asm file with an
    external assembler. *)

(** Support for plumbing a binary code emitter *)

val register_internal_assembler: (asm_program -> string -> unit) -> unit


(** Hooks for rewriting the assembly code *)

val assembler_passes: (asm_program -> asm_program) list ref
