(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction selection for the Alpha processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = digital_asm || (n >= 0 && n <= 255)

method select_addressing = function
    (* Force an explicit lda for non-scheduling assemblers,
       this allows our scheduler to do a better job. *)
    Cconst_symbol s when digital_asm ->
      (Ibased(s, 0), Ctuple [])
  | Cop((Cadda | Caddi), [Cconst_symbol s; Cconst_int n]) when digital_asm ->
      (Ibased(s, n), Ctuple [])
  | Cop((Cadda | Caddi), [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop((Cadda | Caddi), [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method select_operation op args =
  match (op, args) with
    (* Recognize shift-add operations *)
    ((Caddi|Cadda),
     [arg2; Cop(Clsl, [arg1; Cconst_int(2|3 as shift)])]) ->
      (Ispecific(if shift = 2 then Iadd4 else Iadd8), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [arg1; Cconst_int(4|8 as mult)])]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), [arg1; arg2])
  | ((Caddi|Cadda),
     [arg2; Cop(Cmuli, [Cconst_int(4|8 as mult); arg1])]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), [arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst_int(2|3 as shift)]); arg2]) ->
      (Ispecific(if shift = 2 then Iadd4 else Iadd8), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [arg1; Cconst_int(4|8 as mult)]); arg2]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), [arg1; arg2])
  | (Caddi, [Cop(Cmuli, [Cconst_int(4|8 as mult); arg1]); arg2]) ->
      (Ispecific(if mult = 4 then Iadd4 else Iadd8), [arg1; arg2])
  | (Csubi, [Cop(Clsl, [arg1; Cconst_int(2|3 as shift)]); arg2]) ->
      (Ispecific(if shift = 2 then Isub4 else Isub8), [arg1; arg2])
  | (Csubi, [Cop(Cmuli, [Cconst_int(4|8 as mult); arg1]); arg2]) ->
      (Ispecific(if mult = 4 then Isub4 else Isub8), [arg1; arg2])
    (* Recognize truncation/normalization of 64-bit integers to 32 bits *)
  | (Casr, [Cop(Clsl, [arg; Cconst_int 32]); Cconst_int 32]) ->
      (Ispecific Itrunc32, [arg])
    (* Work around various limitations of the GNU assembler *)
  | ((Caddi|Cadda), [arg1; Cconst_int n])
    when not (self#is_immediate n) && self#is_immediate (-n) ->
      (Iintop_imm(Isub, -n), [arg1])
  | (Cdivi, [arg1; Cconst_int n])
    when (not digital_asm) && n <> 1 lsl (Misc.log2 n) ->
      (Iintop Idiv, args)
  | (Cmodi, [arg1; Cconst_int n])
    when (not digital_asm) && n <> 1 lsl (Misc.log2 n) ->
      (Iintop Imod, args)
  | _ ->
      super#select_operation op args

end

let fundecl f = (new selector)#emit_fundecl f
