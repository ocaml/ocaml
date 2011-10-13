(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction selection for the HPPA processor *)

open Misc
open Cmm
open Reg
open Arch
open Proc
open Mach

let shiftadd = function
    2 -> Ishift1add
  | 4 -> Ishift2add
  | 8 -> Ishift3add
  | _ -> fatal_error "Proc_hppa.shiftadd"

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = (n < 16) && (n >= -16) (* 5 bits *)

method select_addressing = function
    Cconst_symbol s ->
      (Ibased(s, 0), Ctuple [])
  | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadda, [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args =
  match (op, args) with
  (* Recognize shift-add operations. *)
    ((Caddi|Cadda),
     [arg2; Cop(Clsl, [arg1; Cconst_int(1|2|3 as shift)])]) ->
      (Ispecific(shiftadd(1 lsl shift)), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [arg1; Cconst_int(2|4|8 as mult)])]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg1])]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst_int(1|2|3 as shift)]); arg2]) ->
      (Ispecific(shiftadd(1 lsl shift)), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [arg1; Cconst_int(2|4|8 as mult)]); arg2]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg1]); arg2]) ->
      (Ispecific(shiftadd mult), [arg1; arg2])
  (* Prevent the recognition of some immediate arithmetic operations *)
  (* Cmuli : -> Ilsl if power of 2
     Cdivi, Cmodi : only if power of 2
     Cand, Cor, Cxor : never *)
  | (Cmuli, ([arg1; Cconst_int n] as args)) ->
      let l = Misc.log2 n in
      if n = 1 lsl l
      then (Iintop_imm(Ilsl, l), [arg1])
      else (Iintop Imul, args)
  | (Cmuli, ([Cconst_int n; arg1] as args)) ->
      let l = Misc.log2 n in
      if n = 1 lsl l
      then (Iintop_imm(Ilsl, l), [arg1])
      else (Iintop Imul, args)
  | (Cmuli, args) -> (Iintop Imul, args)
  | (Cdivi, [arg1; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg1])
  | (Cdivi, args) -> (Iintop Idiv, args)
  | (Cmodi, [arg1; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Imod, n), [arg1])
  | (Cmodi, args) -> (Iintop Imod, args)
  | (Cand, args) -> (Iintop Iand, args)
  | (Cor, args) -> (Iintop Ior, args)
  | (Cxor, args) -> (Iintop Ixor, args)
  | _ ->
      super#select_operation op args

(* Deal with register constraints *)

method! insert_op_debug op dbg rs rd =
  match op with
    Iintop(Idiv | Imod) -> (* handled via calls to millicode *)
      let rs' = [|phys_reg 20; phys_reg 19|] (* %r26, %r25 *)
      and rd' = [|phys_reg 22|] (* %r29 *) in
      self#insert_moves rs rs';
      self#insert_debug (Iop op) dbg rs' rd';
      self#insert_moves rd' rd;
      rd
  | _ ->
      super#insert_op_debug op dbg rs rd

end

let fundecl f = (new selector)#emit_fundecl f
