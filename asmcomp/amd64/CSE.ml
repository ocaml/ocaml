(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* CSE for the AMD64 *)

open Arch
open Mach
open CSEgen

class cse = object (self)

inherit cse_generic as super

method! class_of_operation op =
  match op with
  | Ispecific(Ilea _) -> Op_pure
  | Ispecific(Istore_int(_, _, is_asg)) -> Op_store is_asg
  | Ispecific(Istore_symbol(_, _, is_asg)) -> Op_store is_asg
  | Ispecific(Ioffset_loc(_, _)) -> Op_store true
  | Ispecific(Ifloatarithmem _) -> Op_load
  | _ -> super#class_of_operation op

end

let fundecl f =
  (new cse)#fundecl f

