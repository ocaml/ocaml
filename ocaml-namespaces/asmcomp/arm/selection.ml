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
open Proc
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

(* Soft emulation of float comparisons *)

let float_comparison_function = function
  | Ceq -> "__eqdf2"
  | Cne -> "__nedf2"
  | Clt -> "__ltdf2"
  | Cle -> "__ledf2"
  | Cgt -> "__gtdf2"
  | Cge -> "__gedf2"

(* Instruction selection *)
class selector = object(self)

inherit Selectgen.selector_generic as super

method! regs_for tyv =
  (* Expand floats into pairs of integer registers *)
  let nty = Array.length tyv in
  let rec expand i =
    if i >= nty then [] else begin
      match tyv.(i) with
      | Float -> Int :: Int :: expand (i+1)
      | ty -> ty :: expand (i+1)
    end in
  Reg.createv (Array.of_list (expand 0))

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

method! select_operation op args =
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
  | Ccheckbound _ ->
      begin match args with
        [Cop(Clsr, [arg1; Cconst_int n]); arg2]
        when n > 0 && n < 32 && not(is_intconst arg2) ->
          (Ispecific(Ishiftcheckbound n), [arg1; arg2])
      | _ ->
        super#select_operation op args
      end
  (* Turn floating-point operations into library function calls *)
  | Caddf -> (Iextcall("__adddf3", false), args)
  | Csubf -> (Iextcall("__subdf3", false), args)
  | Cmulf -> (Iextcall("__muldf3", false), args)
  | Cdivf -> (Iextcall("__divdf3", false), args)
  | Cfloatofint -> (Iextcall("__floatsidf", false), args)
  | Cintoffloat -> (Iextcall("__fixdfsi", false), args)
  | Ccmpf comp ->
      (Iintop_imm(Icomp(Isigned comp), 0),
       [Cop(Cextcall(float_comparison_function comp,
                     typ_int, false, Debuginfo.none),
            args)])
  (* Add coercions around loads and stores of 32-bit floats *)
  | Cload Single ->
      (Iextcall("__extendsfdf2", false), [Cop(Cload Word, args)])
  | Cstore Single ->
      begin match args with
      | [arg1; arg2] ->
          let arg2' =
            Cop(Cextcall("__truncdfsf2", typ_int, false, Debuginfo.none),
                [arg2]) in
          self#select_operation (Cstore Word) [arg1; arg2']
      | _ -> assert false
      end
  (* Other operations are regular *)
  | _ -> super#select_operation op args

method! select_condition = function
  | Cop(Ccmpf cmp, args) ->
      (Iinttest_imm(Isigned cmp, 0),
       Cop(Cextcall(float_comparison_function cmp,
                    typ_int, false, Debuginfo.none),
           args))
  | expr ->
      super#select_condition expr

(* Deal with some register irregularities:

1- In mul rd, rm, rs,  the registers rm and rd must be different.
   We deal with this by pretending that rm is also a result of the mul
   operation.

2- For Inegf and Iabsf, force arguments and results in (r0, r1);
   this simplifies code generation later.
*)

method! insert_op_debug op dbg rs rd =
  match op with
  | Iintop(Imul) ->
      self#insert_debug (Iop op) dbg rs [| rd.(0); rs.(0) |]; rd
  | Iabsf | Inegf ->
      let r = [| phys_reg 0; phys_reg 1 |] in
      self#insert_moves rs r;
      self#insert_debug (Iop op) dbg r r;
      self#insert_moves r rd;
      rd
  | _ ->
      super#insert_op_debug op dbg rs rd

end

let fundecl f = (new selector)#emit_fundecl f
