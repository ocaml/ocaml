(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Reloading for the Motorola 68k *)

open Cmm
open Arch
open Reg
open Mach

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

class reload = struct (self)

inherit Reloadgen.reload_generic as super

method reload_operation op arg res =
  match op with
    Imove | Ireload | Ispill | 
    Iintop_imm((Iadd | Isub | Iand | Ior | Ixor |
                Icomp _ | Ilsl | Ilsr | Iasr), _) |
    Ifloatofint | Iintoffloat | Ispecific(Ipush) ->
      (* The argument(s) can be either in register or on stack *)
      (arg, res)
  | Iintop(Iadd | Isub | Iand | Ior | Ixor | Icomp _) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Ilsl | Ilsr | Iasr) ->
      (* The first argument and result can reside in the stack *)
      ([|arg.(0); self#makereg arg.(1)|], res)
  | Iintop(Imul | Idiv | Imod) | Iaddf | Isubf | Imulf | Idivf ->
      (* The second argument can reside in the stack *)
      let r = self#makereg arg.(0) in ([|r; arg.(1)|], [|r|])
  | _ -> (* Other operations: all args and results in registers *)
      super#reload_operation op arg res

method reload_test tst arg =
  match tst with
    Iinttest _ | Ifloattest _ ->
      (* The second argument can be on stack *)
      [| self#makereg arg.(0); arg.(1) |]
  | _ ->
      (* The argument can be on stack *)
      arg

end

let fundecl f =
  (new reload)#fundecl f
