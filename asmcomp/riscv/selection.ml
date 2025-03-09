# 2 "asmcomp/riscv/selection.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>                 *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the RISC-V processor *)

open Cmm
open Arch
open Mach

(* Instruction selection *)

class selector = object

inherit Selectgen.selector_generic as super

(* RISC-V does not support immediate operands for comparison operators *)
method is_immediate_test _cmp _n = false

method! is_immediate op n =
  match op with
  | Iadd | Iand | Ior | Ixor -> is_immediate n
  (* sub immediate is turned into add immediate opposite *)
  | Isub -> is_immediate (-n)
  | _ -> super#is_immediate op n

method select_addressing _ = function
  | Cop(Cadda, [arg; Cconst_int (n, _)], _) when is_immediate n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int (n, _)], _)], dbg)
    when is_immediate n ->
      (Iindexed n, Cop(Caddi, [arg1; arg2], dbg))
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args dbg =
  match (op, args) with
  (* Recognize (neg-)mult-add and (neg-)mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2], _)]) ->
      (Ispecific (Imultaddf false), [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific (Imultsubf false), [arg1; arg2; arg3])
  | (Cnegf, [Cop(Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3], _)]) ->
      (Ispecific (Imultsubf true), [arg1; arg2; arg3])
  | (Cnegf, [Cop(Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3], _)]) ->
      (Ispecific (Imultaddf true), [arg1; arg2; arg3])
  | (Cstore (Word_int | Word_val as memory_chunk, Assignment), [arg1; arg2]) ->
      (* Use trivial addressing mode for non-initializing stores *)
      (Istore (memory_chunk, Iindexed 0, true), [arg2; arg1])
  | _ ->
      super#select_operation op args dbg

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
