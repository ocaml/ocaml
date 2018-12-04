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

(* Generation of assembly code *)

type external_call_generated_during_emit = private {
  callee : Asm_symbol.t;
  call_labels : Mach.call_labels;
  dbg : Debuginfo.t;
}

val fundecl
   : Linearize.fundecl
  -> end_of_function_label:Linearize.label
  -> external_call_generated_during_emit list

val data: Cmm.data_item list -> unit
val begin_assembly: unit -> unit
val end_assembly: Dwarf.t option -> unit
