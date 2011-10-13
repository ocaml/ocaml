(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Reloading for the HPPA *)


open Cmm
open Arch
open Reg
open Mach
open Proc

class reload = object (self)

inherit Reloadgen.reload_generic as super

method reload_operation op arg res =
  match op with
      Iintop(Idiv | Imod)
    | Iintop_imm((Idiv | Imod), _)  -> (arg, res)
    | _ -> super#reload_operation op arg res
end



let fundecl f =
  (new reload)#fundecl f
