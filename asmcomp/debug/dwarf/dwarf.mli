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
    The names of the parameters line up with the code in [Asmgen].
    The current [Compilation_unit] must have been set before calling this
    function. *)
val create
   : sourcefile:string
  -> prefix_name:string
  -> t

(** Generate DWARF for the given function. *)
val dwarf_for_fundecl : t -> Debug_passes.result -> unit

(** Generate DWARF for Flambda [Let_symbol] bindings. *)
val dwarf_for_toplevel_constants
   : t
  -> Clambda.preallocated_constant list
  -> unit

(** Generate DWARF for Flambda [Initialize_symbol] bindings. *)
val dwarf_for_toplevel_inconstants
   : t
  -> Clambda.preallocated_block list
  -> unit

(** Write the DWARF information to the assembly file.  This should only be
    called once all (in)constants and function declarations have been passed
    to the above functions. *)
val emit : t -> unit

(** Whether support for DWARF-5 (or DWARF-4 plus GNU extensions) call site
    marking is enabled. *)
val supports_call_sites : unit -> bool
