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

(* Representation of machine code by sequences of pseudoinstructions *)

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison * bool
  | Ioddtest
  | Ieventest

type ('addr, 'op) operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of int64
  | Iconst_symbol of string
  | Iconst_blockheader of nativeint
  | Icall_ind
  | Icall_imm of string
  | Itailcall_ind
  | Itailcall_imm of string
  | Iextcall of string * bool    (* false = noalloc, true = alloc *)
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * 'addr
  | Istore of Cmm.memory_chunk * 'addr * bool
                                 (* false = initialization, true = assignment *)
  | Ialloc of int
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of 'op

type ('addr, 'op) instruction =
  { desc: ('addr, 'op) instruction_desc;
    next: ('addr, 'op) instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    mutable live: Reg.Set.t }

and ('addr, 'op) instruction_desc =
    Iend
  | Iop of ('addr, 'op) operation
  | Ireturn
  | Iifthenelse of test * ('addr, 'op) instruction * ('addr, 'op) instruction
  | Iswitch of int array * ('addr, 'op) instruction array
  | Iloop of ('addr, 'op) instruction
  | Icatch of int * ('addr, 'op) instruction * ('addr, 'op) instruction
  | Iexit of int
  | Itrywith of ('addr, 'op) instruction * ('addr, 'op) instruction
  | Iraise of Lambda.raise_kind

type ('addr, 'op) fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: ('addr, 'op) instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t }

val dummy_instr: unit -> ('addr, 'op) instruction
(* val end_instr: unit -> ('addr, 'op) instruction *)
val instr_cons:
      ('addr, 'op) instruction_desc -> Reg.t array -> Reg.t array -> ('addr, 'op) instruction ->
        ('addr, 'op) instruction
val instr_cons_debug:
      ('addr, 'op) instruction_desc -> Reg.t array -> Reg.t array -> Debuginfo.t ->
        ('addr, 'op) instruction -> ('addr, 'op) instruction
val instr_iter: (('addr, 'op) instruction -> unit) -> ('addr, 'op) instruction -> unit
