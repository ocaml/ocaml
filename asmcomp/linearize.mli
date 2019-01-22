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

type label = Cmm.label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t;
    phantom_available_before: Backend_var.Set.t;
    mutable available_before: Reg_availability_set.t;
    mutable available_across: Reg_availability_set.t option;
  }

and instruction_desc =
  | Lprologue
  | Lend
  | Lop of Mach.operation
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
  | Lraise of Cmm.raise_kind

val has_fallthrough :  instruction_desc -> bool
val end_instr: instruction

val instr_cons
   : instruction_desc
  -> Reg.t array
  -> Reg.t array
  -> instruction
  -> available_before:Reg_availability_set.t
  -> phantom_available_before:Ident.Set.t
  -> available_across:Reg_availability_set.t option
  -> Debuginfo.t
  -> instruction

val instr_cons_same_avail
   : instruction_desc
  -> Reg.t array
  -> Reg.t array
  -> instruction
  -> Debuginfo.t
  -> instruction

val invert_test: Mach.test -> Mach.test

type fundecl =
  { fun_name: Backend_sym.t;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.Function.t;
    fun_arity : int;
    fun_spacetime_shape : Mach.spacetime_shape option;
    fun_phantom_lets :
      (Backend_var.Provenance.t option * Mach.phantom_defining_expr)
        Backend_var.Map.t;
  }

val fundecl: Mach.fundecl -> fundecl
