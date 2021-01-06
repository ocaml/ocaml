(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

(* Accessors for the table of toplevel value bindings.  These functions
   must appear as first and second exported functions in this module.
   (See module Translmod.) *)
val getvalue : string -> Obj.t
val setvalue : string -> Obj.t -> unit

(* Read and execute a script from the given file *)

val run_script : formatter -> string -> string array -> bool
        (* true if successful, false if error *)

val execute_phrase : bool -> formatter -> Parsetree.toplevel_phrase -> bool
        (* Execute the given toplevel phrase. Return [true] if the
           phrase executed with no errors and [false] otherwise.
           First bool says whether the values and types of the results
           should be printed. Uncaught exceptions are always printed. *)
val use_file : formatter -> string -> bool
val use_output : formatter -> string -> bool
val use_silently : formatter -> string -> bool
val mod_use_file : formatter -> string -> bool
        (* Read and execute commands from a file.
           [use_file] prints the types and values of the results.
           [use_silently] does not print them.
           [mod_use_file] wrap the file contents into a module. *)
val eval_module_path: Env.t -> Path.t -> Obj.t
val eval_value_path: Env.t -> Path.t -> Obj.t
val eval_extension_path: Env.t -> Path.t -> Obj.t
val eval_class_path: Env.t -> Path.t -> Obj.t
        (* Return the toplevel object referred to by the given path *)

val eval_address: Env.address -> Obj.t
        (* Used for printers *)

val may_trace : bool ref

include Topcommon.PRINTER with type Printer.t = Obj.t

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : bool -> formatter -> string -> bool

val init: unit -> unit
