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

open Cmm
open Arch
open Reg
open Mach

(* Reloading for the Intel x86 *)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

class reload () as self =

inherit Reloadgen.reload_generic () as super

method makereg r =
  match r.typ with
    Float -> r
  | _ -> super#makereg r

(* By overriding makereg, we make sure that pseudoregs of type float
   will never be reloaded. Hence there is no need to make special cases for
   floating-point operations. *)

method reload_operation op arg res =
  match op with
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor|Icomp _|Icheckbound) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Ilsl|Ilsr|Iasr) | Iintop_imm(_, _) | Ifloatofint | Iintoffloat |
    Ispecific(Ipush) ->
      (* The argument(s) can be either in register or on stack *)
      (arg, res)
  | _ -> (* Other operations: all args and results in registers *)
      super#reload_operation op arg res

method reload_test tst arg =
  match tst with
    Iinttest cmp ->
      (* One of the two arguments can reside on stack *)
      if stackp arg.(0) && stackp arg.(1)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | _ ->
      (* The argument(s) can be either in register or on stack *)
      arg

end

let fundecl f =
  (new reload ())#fundecl f


