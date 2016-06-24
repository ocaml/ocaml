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

(* Pretty-printing of pseudo machine code *)

open Format

val reg: (int -> string) -> formatter -> Reg.t -> unit
val regs: (int -> string) -> formatter -> Reg.t array -> unit
val regset: (int -> string) -> formatter -> Reg.Set.t -> unit
val regsetaddr: (int -> string) -> formatter -> Reg.Set.t -> unit
val operation: (int -> string) -> ((formatter -> Reg.t -> unit) -> 'addr -> formatter -> Reg.t array -> unit) -> ((formatter -> Reg.t -> unit) -> 'op -> formatter -> Reg.t array -> unit) -> ('addr, 'op) Mach.operation -> Reg.t array -> formatter -> Reg.t array -> unit
val test: (int -> string) -> Mach.test -> formatter -> Reg.t array -> unit
val instr: (int -> string) -> ((formatter -> Reg.t -> unit) -> 'addr -> formatter -> Reg.t array -> unit) -> ((formatter -> Reg.t -> unit) -> 'op -> formatter -> Reg.t array -> unit) -> formatter -> ('addr, 'op) Mach.instruction -> unit
val fundecl: (int -> string) -> ((formatter -> Reg.t -> unit) -> 'addr -> formatter -> Reg.t array -> unit) -> ((formatter -> Reg.t -> unit) -> 'op -> formatter -> Reg.t array -> unit) -> formatter -> ('addr, 'op) Mach.fundecl -> unit
val phase: string -> (int -> string) -> ((formatter -> Reg.t -> unit) -> 'addr -> formatter -> Reg.t array -> unit) -> ((formatter -> Reg.t -> unit) -> 'op -> formatter -> Reg.t array -> unit) -> formatter -> ('addr, 'op) Mach.fundecl -> unit
val interferences: (int -> string) -> formatter -> unit -> unit
val preferences: (int -> string) -> formatter -> unit -> unit

val print_live: bool ref
