(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Handling of load path for dynamically-linked libraries *)

(* Read the [ld.conf] file and return the corresponding list of directories *)
val ld_conf_contents: unit -> string list

(* Split the CAML_LD_LIBRARY_PATH environment variable and return
   the corresponding list of directories *)
val ld_library_path_contents: unit -> string list

(* Split the given 0-separated path *)
val split_dll_path: string -> string list
