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

type internal_affinity =
  | Previous
  | Irrelevant

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    live: Reg.Set.t;
    mutable dbg : Insn_debuginfo.t;
    affinity : internal_affinity;
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

type affinity =
  | Previous
  | Next

(** Cons an instruction, optionally given its arguments and results, with
    [live] being set to empty.  The [affinity] argument controls whether the
    new instruction's debuginfo should be taken from the next instruction or
    the previous instruction (which will be consed onto this instruction at
    some point in the future).  The setting of this argument should be made
    carefully so as to ensure correct visibility of variables in the
    debugger. *)
val cons_instr
   : ?arg:Reg.t array
  -> ?res:Reg.t array
  -> affinity
  -> instruction_desc
  -> instruction
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
