(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

open Unix

val   add_fileinput : fd:file_descr -> callback:(unit -> unit) -> unit
val   remove_fileinput: fd:file_descr -> unit
val   add_fileoutput : fd:file_descr -> callback:(unit -> unit) -> unit
val   remove_fileoutput: fd:file_descr -> unit
      (* see [tk] module *)
