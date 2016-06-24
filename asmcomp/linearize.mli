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

(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = int
val new_label: unit -> label

type ('addr, 'op) instruction =
  { mutable desc: ('addr, 'op) instruction_desc;
    mutable next: ('addr, 'op) instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t }

and ('addr, 'op) instruction_desc =
    Lend
  | Lop of ('addr, 'op) Mach.operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise of Lambda.raise_kind

val has_fallthrough :  ('addr, 'op) instruction_desc -> bool
val end_instr: unit -> ('addr, 'op) instruction
val instr_cons:
  ('addr, 'op) instruction_desc -> Reg.t array -> Reg.t array -> ('addr, 'op) instruction -> ('addr, 'op) instruction
val invert_test: Mach.test -> Mach.test

type ('addr, 'op) fundecl =
  { fun_name: string;
    fun_body: ('addr, 'op) instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t }

val reset : unit -> unit
val fundecl: ('addr, 'op) Mach.fundecl -> ('addr, 'op) fundecl
