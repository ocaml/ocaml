(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction selection for the ARM processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Immediate operands are 8-bit immediate values, zero-extended, and rotated
   right by 0, 2, 4, ... 30 bits.
   To avoid problems with Caml's 31-bit arithmetic,
   we check only with 8-bit values shifted left 0 to 22 bits. *)

let rec is_immed n shift =
  if shift > 22 then false
  else if n land (0xFF lsl shift) = n then true
  else is_immed n (shift + 2)

(* We have 12-bit + sign byte offsets for word accesses,
   8-bit + sign word offsets for float accesses,
   and 8-bit + sign byte offsets for bytes and shorts.
   Use lowest common denominator. *)

let is_offset n = n < 256 && n > -256

let is_intconst = function Cconst_int n -> true | _ -> false

(* Instruction selection *)
class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate n =
  n land 0xFF = n || is_immed n 2

method select_addressing = function
    Cop(Cadda, [arg; Cconst_int n]) when is_offset n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) when is_offset n ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method select_shift_arith op shiftop shiftrevop args =
  match args with
    [arg1; Cop(Clsl, [arg2; Cconst_int n])]
    when n > 0 && n < 32 && not(is_intconst arg2) ->
      (Ispecific(Ishiftarith(shiftop, n)), [arg1; arg2])
  | [arg1; Cop(Casr, [arg2; Cconst_int n])]
    when n > 0 && n < 32 && not(is_intconst arg2) ->
      (Ispecific(Ishiftarith(shiftop, -n)), [arg1; arg2])
  | [Cop(Clsl, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 && not(is_intconst arg1) ->
      (Ispecific(Ishiftarith(shiftrevop, n)), [arg2; arg1])
  | [Cop(Casr, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 && not(is_intconst arg1) ->
      (Ispecific(Ishiftarith(shiftrevop, -n)), [arg2; arg1])
  | _ ->
      super#select_operation op args

method select_operation op args =
  match op with
    Cadda | Caddi ->
      begin match args with
        [arg1; Cconst_int n] when n < 0 && self#is_immediate (-n) ->
          (Iintop_imm(Isub, -n), [arg1])
      | _ ->
          self#select_shift_arith op Ishiftadd Ishiftadd args
      end
  | Csuba | Csubi ->
      begin match args with
        [arg1; Cconst_int n] when n < 0 && self#is_immediate (-n) ->
          (Iintop_imm(Iadd, -n), [arg1])
      | [Cconst_int n; arg2] when self#is_immediate n ->
          (Ispecific(Irevsubimm n), [arg2])
      | _ ->
          self#select_shift_arith op Ishiftsub Ishiftsubrev args
      end
  | Cmuli ->                    (* no multiply immediate *)
      (Iintop Imul, args)
  | Cdivi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Idiv, n), [arg1])
      | _ ->
          (Iextcall("__divsi3", false), args)
      end
  | Cmodi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Imod, n), [arg1])
      | _ ->
          (Iextcall("__modsi3", false), args)
      end
  | Ccheckbound ->
      begin match args with
        [Cop(Clsr, [arg1; Cconst_int n]); arg2]
        when n > 0 && n < 32 && not(is_intconst arg2) ->
          (Ispecific(Ishiftcheckbound n), [arg1; arg2])
      | _ ->
        super#select_operation op args
      end
  | _ -> super#select_operation op args

(* In mul rd, rm, rs,  rm and rd must be different.
   We deal with this by pretending that rm is also a result of the mul
   operation. *)

method insert_op op rs rd =
  if op = Iintop(Imul) then begin
    self#insert (Iop op) rs [| rd.(0); rs.(0) |]; rd
  end else
    super#insert_op op rs rd

end

let fundecl f = (new selector)#emit_fundecl f

