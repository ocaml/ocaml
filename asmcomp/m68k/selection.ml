(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction selection for the Motorola 68k *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Csubi | Csuba), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)

(* Special constraints on operand and result registers for two-address
   instructions *)

exception Use_default

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd | Isub | Imul | Idiv | Imod | Ilsl | Ilsr | Iasr) |
    Iaddf | Isubf | Imulf | Idivf ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address binary operations, forcing the second argument to be
     in a data register *)
  | Iintop(Iand | Ior | Ixor) ->
      let newarg1 = Reg.create Int in
      ([|res.(0); newarg1|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor |
                Ilsl | Ilsr | Iasr), _) ->
      (res, res, false)
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* The selector *)
    
class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate (n : int) = true

(* Select addressing modes *)
method select_addressing exp =
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])
  | (Ascale(e, scale), d) ->
      (Iscaled(scale, d), e)
  | (Ascaledadd(e1, e2, scale), d) ->
      (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method select_operation op args =
  match op with
  (* Recognize the LEA instruction *)
    Cadda | Csuba ->
      begin match self#select_addressing (Cop(op, args)) with
        (Iindexed d, _) -> super#select_operation op args
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize immediate shifts only if 1 <= count <= 8 *)
  | Clsl -> self#select_shift op Ilsl args
  | Clsr -> self#select_shift op Ilsr args
  | Casr -> self#select_shift op Iasr args
  | _ -> super#select_operation op args

(* Selection of immediate shifts -- only if count is between 1 and 8 *)

method select_shift cop iop args =
  match args with
    [arg1; Cconst_int n] ->
      if n >= 1 && n <= 8 then
        (Iintop_imm(iop, n), [arg1])
      else if n >= 9 && n <= 16 then
        (Iintop_imm(iop, n - 8), [Cop(cop, [arg1; Cconst_int 8])])
      else
        (Iintop iop, args)
  | args -> (Iintop iop, args)

(* Select store operations *)

method select_store addr exp =
  match exp with
    Cconst_int n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Istore_symbol(s, addr)), Ctuple [])
  | _ -> super#select_store addr exp

(* Deal with register constraints *)

method insert_op op rs rd =
  try
    let (rsrc, rdst, move_res) = pseudoregs_for_operation op rs rd in
    self#insert_moves rs rsrc;
    self#insert (Iop op) rsrc rdst;
    if move_res then begin
      self#insert_moves rdst rd;
      rd
    end else
      rdst
  with Use_default ->
    super#insert_op op rs rd

(* Select push operations for external calls *)

method select_push exp =
  match exp with
    Cconst_int n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Ipush_symbol s), Ctuple [])
  | Cop(Cload ty, [loc]) when ty = typ_float ->
      let (addr, arg) = self#select_addressing loc in
      (Ispecific(Ipush_load_float addr), arg)
  | Cop(Cload ty, [loc]) when ty = typ_addr or ty = typ_int ->
      let (addr, arg) = self#select_addressing loc in
      (Ispecific(Ipush_load addr), arg)
  | _ -> (Ispecific(Ipush), exp)

method emit_extcall_args env args =
  let rec emit_pushes = function
    [] -> 0
  | e :: el ->
      let ofs = emit_pushes el in
      let (op, arg) = self#select_push e in
      let r = self#emit_expr env arg in
      self#insert (Iop op) r [||];
      ofs + Selectgen.size_expr env e
  in ([||], emit_pushes args)

end

let fundecl f = (new selector)#emit_fundecl f
