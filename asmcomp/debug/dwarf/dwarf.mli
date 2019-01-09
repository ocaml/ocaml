(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generation and emission of DWARF debugging information for OCaml
    compilation units. *)

type t

(** Create a value of type [t], which holds all state necessary to emit
    DWARF debugging information for a single compilation unit.
    The names of certain parameters line up with the code in [Asmgen].
    The current [Compilation_unit] must have been set before calling this
    function. *)
val create
   : sourcefile:string
  -> prefix_name:string
  -> cmt_file_digest:Digest.t option
  -> objfiles:string list
  -> t

(** Generate DWARF for the given function. *)
val dwarf_for_fundecl : t -> Debug_passes.result -> unit

(** Generate DWARF for Flambda [Let_symbol] bindings. *)
val dwarf_for_toplevel_constants
   : t
  -> Clambda.preallocated_constant list
  -> unit

(** For dealing with [Closure]'s top level module blocks.  The symbol for
    the module block and the corresponding variable must be provided. *)
val dwarf_for_closure_top_level_module_block
   : t
  -> module_block_sym:Backend_sym.t
  -> module_block_var:Backend_var.t
  -> unit

(** Write the DWARF information to the assembly file.  This should only be
    called once all (in)constants and function declarations have been passed
    to the above functions. *)
val emit : t -> unit
