(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open Arch
open Cmm
open Mach

let is_offset chunk n =
   (n >= -256 && n <= 255)               (* 9 bits signed unscaled *)
|| (n >= 0 &&
    match chunk with     (* 12 bits unsigned, scaled by chunk size *)
    | Byte_unsigned | Byte_signed ->
        n < 0x1000
    | Sixteen_unsigned | Sixteen_signed ->
        n land 1 = 0 && n lsr 1 < 0x1000
    | Thirtytwo_unsigned | Thirtytwo_signed | Single ->
        n land 3 = 0 && n lsr 2 < 0x1000
    | Word_int | Word_val | Double ->
        n land 7 = 0 && n lsr 3 < 0x1000)

let is_logical_immediate n =
  Arch.is_logical_immediate (Nativeint.of_int n)

(* Signed immediates are simpler *)

let is_immediate n =
  let mn = -n in
  n land 0xFFF = n || n land 0xFFF_000 = n
  || mn land 0xFFF = mn || mn land 0xFFF_000 = mn

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops =
  [ "sqrt"; "caml_bswap16_direct"; "caml_int32_direct_bswap";
    "caml_int64_direct_bswap"; "caml_nativeint_direct_bswap" ]

let use_direct_addressing _symb =
  (not !Clflags.dlcode) && (not Arch.macosx)

let is_stack_slot rv =
  Reg.(match rv with
        | [| { loc = Stack _ } |] -> true
        | _ -> false)

(* Instruction selection *)

class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate_test _cmp n =
  is_immediate n

method! is_immediate op n =
  match op with
  | Iadd | Isub  -> n <= 0xFFF_FFF && n >= -0xFFF_FFF
  | Iand | Ior | Ixor -> is_logical_immediate n
  | Icomp _ | Icheckbound -> is_immediate n
  | _ -> super#is_immediate op n

method! is_simple_expr = function
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      List.for_all self#is_simple_expr args
  | e -> super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | e -> super#effects_of e

method select_addressing chunk = function
  | Cop((Caddv | Cadda), [Cconst_symbol (s, _); Cconst_int (n, _)], _)
    when use_direct_addressing s ->
      (Ibased(s, n), Ctuple [])
  | Cop((Caddv | Cadda), [arg; Cconst_int (n, _)], _)
    when is_offset chunk n ->
      (Iindexed n, arg)
  | Cop((Caddv | Cadda as op),
      [arg1; Cop(Caddi, [arg2; Cconst_int (n, _)], _)], dbg)
    when is_offset chunk n ->
      (Iindexed n, Cop(op, [arg1; arg2], dbg))
  | Cconst_symbol (s, _)
    when use_direct_addressing s ->
      (Ibased(s, 0), Ctuple [])
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args dbg =
  match op with
  (* Integer addition *)
  | Caddi | Caddv | Cadda ->
      begin match args with
      (* Shift-add *)
      | [arg1; Cop(Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg1; arg2])
      | [arg1; Cop(Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg1; arg2])
      | [Cop(Clsl, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg2; arg1])
      | [Cop(Casr, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg2; arg1])
      (* Multiply-add *)
      | [arg1; Cop(Cmuli, args2, dbg)] | [Cop(Cmuli, args2, dbg); arg1] ->
          begin match self#select_operation Cmuli args2 dbg with
          | (Iintop_imm(Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftadd, l)), [arg1; arg3])
          | (Iintop Imul, [arg3; arg4]) ->
              (Ispecific Imuladd, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Integer subtraction *)
  | Csubi ->
      begin match args with
      (* Shift-sub *)
      | [arg1; Cop(Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, n)), [arg1; arg2])
      | [arg1; Cop(Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, -n)), [arg1; arg2])
      (* Multiply-sub *)
      | [arg1; Cop(Cmuli, args2, dbg)] ->
          begin match self#select_operation Cmuli args2 dbg with
          | (Iintop_imm(Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftsub, l)), [arg1; arg3])
          | (Iintop Imul, [arg3; arg4]) ->
              (Ispecific Imulsub, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Checkbounds *)
  | Ccheckbound ->
      begin match args with
      | [Cop(Clsr, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftcheckbound { shift = n; }),
            [arg1; arg2])
      | _ ->
          super#select_operation op args dbg
      end
  (* Recognize sign extension *)
  | Casr ->
      begin match args with
        [Cop(Clsl, [k; Cconst_int (n, _)], _); Cconst_int (n', _)]
        when n' = n && 0 < n && n < 64 ->
          (Ispecific (Isignext (64 - n)), [k])
        | _ -> super#select_operation op args dbg
      end
  (* Recognize floating-point negate and multiply *)
  | Cnegf ->
      begin match args with
      | [Cop(Cmulf, args, _)] -> (Ispecific Inegmulf, args)
      | _ -> super#select_operation op args dbg
      end
  (* Recognize floating-point multiply and add/sub *)
  | Caddf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Imuladdf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  | Csubf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] ->
          (Ispecific Imulsubf, arg :: args)
      | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Inegmulsubf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  (* Recognize floating-point square root *)
  | Cextcall("sqrt", _, _, _) ->
      (Ispecific Isqrtf, args)
  (* Recognize bswap instructions *)
  | Cextcall("caml_bswap16_direct", _, _, _) ->
      (Ispecific(Ibswap 16), args)
  | Cextcall("caml_int32_direct_bswap", _, _, _) ->
      (Ispecific(Ibswap 32), args)
  | Cextcall(("caml_int64_direct_bswap"|"caml_nativeint_direct_bswap"),
              _, _, _) ->
      (Ispecific (Ibswap 64), args)
  (* Other operations are regular *)
  | _ ->
      super#select_operation op args dbg

method! insert_move_extcall_arg env ty_arg src dst =
  if macosx && ty_arg = XInt32 && is_stack_slot dst
  then self#insert env (Iop (Ispecific Imove32)) src dst
  else self#insert_moves env src dst
end

let fundecl ~future_funcnames f = (new selector)#emit_fundecl
                                            ~future_funcnames f
