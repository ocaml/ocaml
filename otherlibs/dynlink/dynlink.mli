(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Dynlink]: dynamic loading of bytecode object files *)

val init : unit -> unit
        (* Initialize the library. Must be called before [loadfile]. *)
val loadfile : string -> unit
        (* Load the given bytecode object file and link it.
           All toplevel expressions in the loaded compilation unit
           are evaluated. No facilities are provided to
           access value names defined by the unit. Therefore, the unit
           must register itself its entry points with the main program,
           e.g. by modifying tables of functions. *)
val loadfile_private : string -> unit
        (* Same as [loadfile], except that the module loaded is not
           made available to other modules dynamically loaded afterwards. *)
val add_interfaces : string list -> string list -> unit
        (* [add_interfaces units path] grants dynamically-linked object
           files access to the compilation  units named in list [units].
           The interfaces ([.cmi] files) for these units are searched in
           [path] (a list of directory names). Initially, dynamically-linked
           object files do not have access to any of the compilation
           units composing the running program, not even the standard library.
           [add_interfaces] or [add_available_units] (see below) must be
           called to grant access to some of the units. *)
val add_available_units : (string * Digest.t) list -> unit
        (* Same as [add_interfaces], but instead of searching [.cmi] files
           to find the unit interfaces, uses the interface digests given
           for each unit. This way, the [.cmi] interface files need not be
           available at run-time. The digests can be extracted from [.cmi]
           files using the [extract_crc] program installed in the
           Objective Caml standard library directory. *)
val clear_available_units : unit -> unit
        (* Clear the list of compilation units accessible to dynamically-linked
           programs. *)
val allow_unsafe_modules : bool -> unit
        (* Govern whether unsafe object files are allowed to be
           dynamically linked. A compilation unit is ``unsafe'' if it contains
           declarations of external functions, which can break type safety.
           By default, dynamic linking of unsafe object files is
           not allowed. *)

type linking_error =
    Undefined_global of string
  | Unavailable_primitive of string
type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
exception Error of error
        (* Errors in dynamic linking are reported by raising the [Error]
           exception with a description of the error. *)
val error_message: error -> string
        (* Convert an error description to a printable message. *)

(*--*)

val digest_interface : string -> string list -> Digest.t
