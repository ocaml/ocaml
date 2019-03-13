(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Dynamic loading of .cmo, .cma and .cmxs files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val is_native : bool
(** [true] if the program is native,
    [false] if the program is bytecode. *)

(** {1 Dynamic loading of compiled files} *)

val loadfile : string -> unit
(** In bytecode: load the given bytecode object file ([.cmo] file) or
    bytecode library file ([.cma] file), and link it with the running
    program. In native code: load the given OCaml plugin file (usually
    [.cmxs]), and link it with the running program.

    All toplevel expressions in the loaded compilation units
    are evaluated. No facilities are provided to
    access value names defined by the unit. Therefore, the unit
    must itself register its entry points with the main program (or a
    previously-loaded library) e.g. by modifying tables of functions.

    An exception will be raised if the given library defines toplevel
    modules whose names clash with modules existing either in the main
    program or a shared library previously loaded with [loadfile].
    Modules from shared libraries previously loaded with
    [loadfile_private] are not included in this restriction.

    The compilation units loaded by this function are added to the
    "allowed units" list (see {!set_allowed_units}). *)

val loadfile_private : string -> unit
(** Same as [loadfile], except that the compilation units just loaded
    are hidden (cannot be referenced) from other modules dynamically
    loaded afterwards.

    An exception will be raised if the given library defines toplevel
    modules whose names clash with modules existing in either the main
    program or a shared library previously loaded with [loadfile].
    Modules from shared libraries previously loaded with
    [loadfile_private] are not included in this restriction.

    An exception will also be raised if the given library defines
    toplevel modules whose name matches that of an interface depended
    on by a module existing in either the main program or a shared
    library previously loaded with [loadfile]. This applies even if
    such dependency is only a "module alias" dependency (i.e. just on
    the name rather than the contents of the interface).

    The compilation units loaded by this function are not added to the
    "allowed units" list (see {!set_allowed_units}) since they cannot
    be referenced from other compilation units. *)

val adapt_filename : string -> string
(** In bytecode, the identity function. In native code, replace the last
    extension with [.cmxs]. *)

(** {1 Access control} *)

val set_allowed_units : string list -> unit
(** Set the list of compilation units that may be referenced from units that
    are dynamically loaded in the future to be exactly the given value.

    Initially all compilation units composing the program currently running
    are available for reference from dynamically-linked units.
    [set_allowed_units] can be used to restrict access to a subset of these
    units, e.g. to the units that compose the API for
    dynamically-linked code, and prevent access to all other units,
    e.g. private, internal modules of the running program.

    Note that {!loadfile} changes the allowed-units list. *)

val allow_only: string list -> unit
(** [allow_only units] sets the list of allowed units to be the intersection
    of the existing allowed units and the given list of units.  As such it
    can never increase the set of allowed units. *)

val prohibit : string list -> unit
(** [prohibit units] prohibits dynamically-linked units from referencing
    the units named in list [units] by removing such units from the allowed
    units list.  This can be used to prevent access to selected units,
    e.g. private, internal modules of the running program. *)

val main_program_units : unit -> string list
(** Return the list of compilation units that form the main program (i.e.
    are not dynamically linked). *)

val public_dynamically_loaded_units : unit -> string list
(** Return the list of compilation units that have been dynamically loaded via
    [loadfile] (and not via [loadfile_private]).  Note that compilation units
    loaded dynamically cannot be unloaded. *)

val all_units : unit -> string list
(** Return the list of compilation units that form the main program together
    with those that have been dynamically loaded via [loadfile] (and not via
    [loadfile_private]). *)

val allow_unsafe_modules : bool -> unit
(** Govern whether unsafe object files are allowed to be
    dynamically linked. A compilation unit is 'unsafe' if it contains
    declarations of external functions, which can break type safety.
    By default, dynamic linking of unsafe object files is
    not allowed. In native code, this function does nothing; object files
    with external functions are always allowed to be dynamically linked. *)

(** {1 Error reporting} *)

type linking_error = private
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = private
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error of error
(** Errors in dynamic linking are reported by raising the [Error]
    exception with a description of the error.
    A common case is the dynamic library not being found on the system: this
    is reported via [Cannot_open_dynamic_library] (the enclosed exception may
    be platform-specific). *)

val error_message : error -> string
(** Convert an error description to a printable message. *)

(**/**)

val unsafe_get_global_value : bytecode_or_asm_symbol:string -> Obj.t option
(** Obtain the globally-visible value whose address is that of the given symbol.
    The symbol name must be the mangled form as would occur in bytecode or
    a native object file.  [None] is returned if the value is inaccessible.
    The accessible values are those in the main program and those provided by
    previous calls to [loadfile].

    This function is deemed "unsafe" as there is no type safety provided.

    When executing in bytecode, this function uses [Symtable]. As a cautionary
    note for programs such as the debugger: even though the linking of a packed
    (subset of) compilerlibs into [Dynlink] hides the copy of [Symtable] that
    [Dynlink] uses from its clients, there is still only one table of global
    values in the bytecode VM. Changes to this table are NOT synchronized
    between [Dynlink] and the functions that change the global value table
    ([update_global_table] and [assign_global_value], accessed through a
    client's version of [Symtable]). This is why we can't use [Dynlink] from the
    toplevel interactive loop, in particular.
*)
