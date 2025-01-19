(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of primitives from dynamically-linked libraries *)

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)
val extract_dll_name: string -> string

(* Open a list of DLLs.  Raise [Failure msg] in case of error. *)
val open_dlls: string list -> unit

(* [have_primitive name] is [true] if one of the currently opened DLLs defines
   the primitive [name]. *)
val have_primitive: string -> bool

(* Add the given directories at the head of the search path for DLLs *)
val add_path: string list -> unit

(* Initialization for separate compilation.
   Initialize the DLL search path to the directories given in the
   environment variable CAML_LD_LIBRARY_PATH, plus contents of ld.conf file
   if argument is [false].  If argument is [true], ignore ld.conf. *)
val init_compile: bool -> unit

val reset: unit -> unit
