(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the Power PC processor *)

open Cmm
open Arch
open Mach

(* Recognition of addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cconst_symbol (s, _) ->
      (Asymbol s, 0, Debuginfo.none)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], dbg) ->
      let (a, n, _) = select_addr arg in (a, n + m, dbg)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], dbg) ->
      let (a, n, _) = select_addr arg in (a, n + m, dbg)
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], dbg) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1, _), (Alinear e2, n2, _)) ->
              (Aadd(e1, e2), n1 + n2, dbg)
        | _ ->
              (Aadd(arg1, arg2), 0, dbg)
      end
  | exp ->
      (Alinear exp, 0, Debuginfo.none)

let is_immediate n = n <= 0x7FFF && n >= -0x8000
let is_immediate_logical n = n <= 0xFFFF && n >= 0

(* Instruction selection *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate_test cmp n =
  match cmp with
  | Isigned _ -> is_immediate n
  | Iunsigned _ -> is_immediate_logical n

method! is_immediate op n =
  match op with
  | Iadd | Imul -> is_immediate n
  | Isub -> is_immediate (-n)  (* turned into add opposite *)
  | Iand | Ior | Ixor -> is_immediate_logical n
  | Icomp c -> self#is_immediate_test c n
  | Icheckbound -> 0 <= n && n <= 0x7FFF
    (* twlle takes a 16-bit signed immediate but performs an unsigned compare *)
  | _ -> super#is_immediate op n

method select_addressing _chunk exp =
  match select_addr exp with
    (Asymbol s, d, _dbg) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d, _dbg) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d, dbg) ->
      if d = 0
      then (Iindexed2, Ctuple[e1; e2])
      else (Iindexed d, Cop(Cadda, [e1; e2], dbg))

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

end

let fundecl ~future_funcnames f =
  (new selector)#emit_fundecl ~future_funcnames f
