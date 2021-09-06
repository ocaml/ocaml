(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            *)
(*                          Bill O'Farrell, IBM                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the Z processor *)

open Cmm
open Arch
open Mach

(* Recognition of addressing modes *)

exception Use_default

type addressing_expr =
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
  | Cop((Caddi | Cadda | Caddv), [arg; Cconst_int (m, _)], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda | Caddv), [Cconst_int (m, _); arg], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda | Caddv), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | exp ->
      (Alinear exp, 0)

(* Instruction selection *)

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor)  | Iaddf|Isubf|Imulf|Idivf ->
      ([|res.(0); arg.(1)|], res)
  | Ispecific _ ->
    ( [| arg.(0); arg.(1); res.(0) |], [| res.(0) |])
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  |  Iintop_imm((Imul|Iand|Ior|Ixor), _) -> (res, res)
  (* Other instructions are regular *)
  | _ -> raise Use_default

let is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000
let is_immediate_logical n = n <= 0xFFFF_FFFF && n >= 0

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate_test cmp n =
  match cmp with
  | Isigned _ -> is_immediate n
  | Iunsigned _ -> is_immediate_logical n

method! is_immediate op n =
  match op with
  | Iadd | Imul -> is_immediate n
  | Isub -> is_immediate (-n)
  | Iand -> n <= -1 && n >= -0x1_0000_0000
  | Ior | Ixor -> is_immediate_logical n
  | Icomp c -> self#is_immediate_test c n
  | Icheckbound -> is_immediate_logical n (* unsigned comparison *)
  | _ -> super#is_immediate op n

method select_addressing _chunk exp =
  let (a, d) = select_addr exp in
  (* 20-bit signed displacement *)
  if d < 0x80000 && d >= -0x80000 then begin
    match a with
    | Alinear e -> (Iindexed d, e)
    | Aadd(e1, e2) -> (Iindexed2 d, Ctuple [e1; e2])
  end else
    (Iindexed 0, exp)

method! select_operation op args dbg =
  match (op, args) with
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2], _)]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | _ ->
      super#select_operation op args dbg

method! insert_op_debug env op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves env rs rsrc;
    self#insert_debug env (Iop op) dbg rsrc rdst;
    self#insert_moves env rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug env op dbg rs rd

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
