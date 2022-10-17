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

(* Handling of dynamically-linked libraries *)

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)
val extract_dll_name: string -> string


type state

(* Initialization for separate compilation.
   Initialize the DLL search path to the directories given in the
   environment variable CAML_LD_LIBRARY_PATH, plus contents of ld.conf file
   if argument is [false].  If argument is [true], ignore ld.conf. *)
val init: bool -> state

(* Add the given directories at the head of the search path for DLLs *)
val add_path: state -> string list -> unit

(* Remove the given directories from the search path for DLLs *)
val remove_path: state -> string list -> unit

(* Open a DLL.
   Raise [Failure msg] in case of error. *)
val open_dll: state -> string -> unit

(* Check if primitive exists *)
val primitive_exists : state -> string -> bool


module Toplevel : sig

  (* Initialization for linking in core (dynlink or toplevel).
     Initialize the search path to the same path that was used to start
     the running program (CAML_LD_LIBRARY_PATH + directories in executable +
     contents of ld.conf file).  Take note of the DLLs that were opened
     when starting the running program. *)
  val init : string -> unit

  (* Add the given directories at the head of the search path for DLLs *)
  val add_path: string list -> unit

  (* Remove the given directories from the search path for DLLs *)
  val remove_path: string list -> unit


  (* Open a DLL.
     Raise [Failure msg] in case of error. *)
  val open_dll: string -> unit

  (* The abstract type representing C function pointers *)
  type dll_address

  (* Find a primitive in the currently opened DLLs
     and return its address and offset in the primitive table. *)
  val find_primitive: string -> (dll_address * int) option
end
