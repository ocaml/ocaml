(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction selection for the IA64 processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Helper function for add selection *)

let reassociate_add = function
    [Cconst_int n; arg] ->
        [arg; Cconst_int n]
  | [Cop(Caddi, [arg1; Cconst_int n]); arg3] ->
        [Cop(Caddi, [arg1; arg3]); Cconst_int n]
  | [Cop(Caddi, [Cconst_int n; arg1]); arg3] ->
        [Cop(Caddi, [arg1; arg3]); Cconst_int n]
  | [arg1; Cop(Caddi, [Cconst_int n; arg3])] ->
        [Cop(Caddi, [arg1; arg3]); Cconst_int n]
  | [arg1; Cop(Caddi, [arg2; arg3])] ->
        [Cop(Caddi, [arg1; arg2]); arg3]
  | args -> args

(* Helper function for mult-immediate selection *)

let rec count_one_bits n =
  if n = 0 then 0
  else if n land 1 = 0 then count_one_bits (n lsr 1)
  else 1 + count_one_bits (n lsr 1)

class selector = object (self)

inherit Selectgen.selector_generic as super

(* Range of immediate arguments:
     add                14-bit signed
     sub                turned into add
     sub reversed       8-bit signed
     mul                at most 16 "one" bits
     div, mod           powers of 2
     and, or, xor       8-bit signed
     lsl, lsr, asr      6-bit unsigned
     cmp                8-bit signed
   For is_immediate, we put 8-bit signed and treat adds specially
   (selectgen already does the right thing for shifts) *)

method is_immediate n = n >= -128 && n < 128

method is_immediate_add n = n >= -8192 && n < 8192

method select_addressing arg = (Iindexed, arg)

method! select_operation op args =
  let norm_op =
    match op with Cadda -> Caddi | Csuba -> Csubi | _ -> op in
  let norm_args =
    match norm_op with Caddi -> reassociate_add args | _ -> args in
  match (norm_op, norm_args) with
  (* Recognize x + y + 1 and x - y - 1 *)
  | (Caddi, [Cop(Caddi, [arg1; arg2]); Cconst_int 1]) ->
      (Ispecific Iadd1, [arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst_int 1]); Cconst_int 1]) ->
      (Ispecific Iadd1, [arg1])
  | (Csubi, [Cop(Csubi, [arg1; arg2]); Cconst_int 1]) ->
      (Ispecific Isub1, [arg1; arg2])
  | (Csubi, [Cop(Csubi, [arg1; Cconst_int 1]); arg2]) ->
      (Ispecific Isub1, [arg1; arg2])
  (* Recognize add immediate *)
  | (Caddi, [arg; Cconst_int n]) when self#is_immediate_add n ->
      (Iintop_imm(Iadd, n), [arg])
  (* Turn sub immediate into add immediate *)
  | (Csubi, [arg; Cconst_int n]) when self#is_immediate_add (-n) ->
      (Iintop_imm(Iadd, -n), [arg])
  (* Recognize imm - arg *)
  | (Csubi, [Cconst_int n; arg]) when self#is_immediate n ->
      (Iintop_imm(Isub, n), [arg])
  (* Recognize shift-add operations *)
  | (Caddi, [arg2; Cop(Clsl, [arg1; Cconst_int(1|2|3|4 as shift)])]) ->
      (Ispecific(Ishladd shift), [arg1; arg2])
  | (Caddi, [Cop(Clsl, [arg1; Cconst_int(1|2|3|4 as shift)]); arg2]) ->
      (Ispecific(Ishladd shift), [arg1; arg2])
  (* Recognize truncation/normalization of 64-bit integers to 32 bits *)
  | (Casr, [Cop(Clsl, [arg; Cconst_int 32]); Cconst_int 32]) ->
      (Ispecific (Isignextend 4), [arg])
  (* Recognize x * cst and cst * x *)
  | (Cmuli, [arg; Cconst_int n]) ->
      self#select_imul_imm arg n
  | (Cmuli, [Cconst_int n; arg]) ->
      self#select_imul_imm arg n
  (* Prevent the recognition of (x / cst) and (x % cst) when cst is not
     a power of 2, which do not correspond to an instruction.
     Turn general division and modulus into calls to C library functions *)
  | (Cdivi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg])
  | (Cdivi, _) ->
      (Iextcall("__divdi3", false), args)
  | (Cmodi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) && n <> 1 ->
      (Iintop_imm(Imod, n), [arg])
  | (Cmodi, _) ->
      (Iextcall("__moddi3", false), args)
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2])]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | (Csubf, [arg3; Cop(Cmulf, [arg1; arg2])]) ->
      (Ispecific Isubmultf, [arg1; arg2; arg3])
  (* Use default selector otherwise *)
  | _ ->
      super#select_operation op args

method private select_imul_imm arg n =
  if count_one_bits n <= 16
  then (Iintop_imm(Imul, n), [arg])
  else (Iintop Imul, [arg; Cconst_int n])

(* To palliate the lack of addressing with displacement, multiple
   stores to the address r are translated as follows
   (t1 and t2 are two temp regs)
      t1 := r - 8
      t2 := r
      compute data1 in reg1
      compute data2 in reg2
      store reg1 at t1 and increment t1 by 16
      store reg2 at t2 and increment t2 by 16
      compute data3 in reg3
      compute data4 in reg4
      store reg3 at t1 and increment t1 by 16
      store reg4 at t2 and increment t2 by 16
      ...
    Note: we use two temp regs and perform stores by groups of 2
    in order to expose more instruction-level parallelism. *)
method! emit_stores env data regs_addr =
  let t1 = Reg.create Addr and t2 = Reg.create Addr in
  self#insert (Iop(Iintop_imm(Iadd, -8))) regs_addr [|t1|];
  self#insert (Iop Imove) regs_addr [|t2|];
  (* Store components by batch of 2 *)
  let backlog = ref None in
  let do_store r =
    match !backlog with
      None -> (* keep it for later *)
        backlog := Some r
    | Some r' -> (* store r' at t1 and r at t2 *)
        self#insert (Iop(Ispecific(Istoreincr 16))) [| t1; r' |] [| t1 |];
        self#insert (Iop(Ispecific(Istoreincr 16))) [| t2; r  |] [| t2 |];
        backlog := None in
  List.iter
    (fun exp ->
      match self#emit_expr env exp with
        None -> assert false
      | Some regs -> Array.iter do_store regs)
    data;
  (* Store the backlog if any *)
  begin match !backlog with
    None -> ()
  | Some r -> self#insert (Iop(Ispecific(Istoreincr 16))) [| t1; r |] [| t1 |]
  end;
  (* Insert an init barrier *)
  self#insert (Iop(Ispecific Iinitbarrier)) [||] [||]
end

let fundecl f = (new selector)#emit_fundecl f
