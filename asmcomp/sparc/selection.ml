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

(* Instruction selection for the Sparc processor *)

open Cmm
open Reg
open Arch
open Mach

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = (n <= 4095) && (n >= -4096)

method select_addressing _chunk = function
    Cconst_symbol s ->
      (Ibased(s, 0), Ctuple [])
  | Cop((Caddv | Cadda), [Cconst_symbol s; Cconst_int n]) ->
      (Ibased(s, n), Ctuple [])
  | Cop((Caddv | Cadda), [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop((Caddv | Cadda as op), [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(op, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args =
  match (op, args) with
  (* For SPARC V7 multiplication, division and modulus are turned into
     calls to C library routines.
     For SPARC V8 and V9, use hardware multiplication and division,
     but C library routine for modulus. *)
    (Cmuli, _) when !arch_version = SPARC_V7 ->
      (Iextcall(".umul", false), args)
  | (Cdivi, _) when !arch_version = SPARC_V7 ->
      (Iextcall(".div", false), args)
  | (Cmodi, _) ->
      (Iextcall(".rem", false), args)
  | _ ->
      super#select_operation op args

(* Override insert_move_args to deal correctly with floating-point
   arguments being passed into pairs of integer registers. *)
method! insert_move_args arg loc stacksize =
  if stacksize <> 0 then self#insert (Iop(Istackoffset stacksize)) [||] [||];
  let locpos = ref 0 in
  for i = 0 to Array.length arg - 1 do
    let src = arg.(i) in
    let dst = loc.(!locpos) in
    match (src, dst) with
      ({typ = Float}, {typ = Int}) ->
        let dst2 = loc.(!locpos + 1) in
        self#insert (Iop Imove) [|src|] [|dst; dst2|];
        locpos := !locpos + 2
    | (_, _) ->
        self#insert_move src dst;
        incr locpos
  done

end

let fundecl f = (new selector)#emit_fundecl f
