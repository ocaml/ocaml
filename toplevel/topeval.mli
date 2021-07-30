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

(** This module provides two alternative implementations for internals of
    [Toploop], for bytecode and native code.

    You should not use it directly, refer to the functions in [Toploop] instead.
*)

(**/**)

open Format

(* Accessors for the table of toplevel value bindings. For the bytecode
   toplevel, these functions must appear as first and second exported functions
   in this module.
   (See module Translmod.)
   They aren't used for the native toplevel.
*)
val getvalue : string -> Obj.t
val setvalue : string -> Obj.t -> unit

(* Label appended after [OCaml version XXX] when starting the toplevel. *)
val implementation_label: string

val execute_phrase : bool -> formatter -> Parsetree.toplevel_phrase -> bool
        (* Read and execute commands from a file.
           [use_file] prints the types and values of the results.
           [use_silently] does not print them.
           [mod_use_file] wrap the file contents into a module. *)

val may_trace : bool ref

module EvalBase: Topcommon.EVAL_BASE

include module type of Topcommon.MakeEvalPrinter(EvalBase)

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : bool -> formatter -> string -> bool

val init: unit -> unit
