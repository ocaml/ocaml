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

(* Instruction selection for the Sparc processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Recognition of addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | exp ->
      (Alinear exp, 0)

class selector = struct (self)

inherit Selectgen.selector_generic as super

method is_immediate n = (n <= 4095) && (n >= -4096)

method select_addressing exp =
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])

method select_operation op args =
  match (op, args) with
  (* Multiplication, division and modulus are turned into
     calls to C library routines, except if the dividend is a power of 2. *)
    (Cmuli, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Ilsl, Misc.log2 n), [arg])
  | (Cmuli, [Cconst_int n; arg]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Ilsl, Misc.log2 n), [arg])
  | (Cmuli, _) -> 
      (Iextcall(".umul", false), args)
  | (Cdivi, [arg; Cconst_int n])
    when self#is_immediate n & n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg])
  | (Cdivi, _) -> 
      (Iextcall(".div", false), args)
  | (Cmodi, [arg; Cconst_int n])
    when self#is_immediate n & n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Imod, n), [arg])
  | (Cmodi, _) ->
      (Iextcall(".rem", false), args)
  | _ ->
      super#select_operation op args

(* Override insert_move_args to deal correctly with floating-point
   arguments being passed into pairs of integer registers. *)
method insert_move_args arg loc stacksize =
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
