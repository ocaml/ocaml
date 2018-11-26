(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Reloading for the Z Processor *)

open Arch
open Mach

class reload = object (self)

inherit Reloadgen.reload_generic as super


(* For 2-address instructions, reloading must make sure that the
   temporary result register is the same as the appropriate
   argument register. *)

method! reload_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor)  | Iaddf|Isubf|Imulf|Idivf ->
      let res = self#makereg res.(0) in
      ([|res; self#makereg arg.(1)|], [|res|])
  (* Three-address ternary operations: arg.(2) and res.(0) must be the same *)
  | Ispecific(Imultaddf|Imultsubf) ->
      let res = self#makereg res.(0) in
      ([|self#makereg arg.(0); self#makereg arg.(1); res|], [|res|])
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  |  Iintop_imm((Imul|Iand|Ior|Ixor), _) ->
      let res = self#makereg res.(0) in
      ([|res|], [|res|])
  (* Force specific registers, as explained in s390x/selection.ml *)
  | Iintop (Iclz _) ->
        let force r target_reg =
          if r = target_reg then r
          else self#makereg target_reg in
        let r7 = force arg.(0) (Proc.phys_reg 5) in
        let r8 = force res.(0) (Proc.phys_reg 6) in
        let r9 = force res.(1) (Proc.phys_reg 7) in
        ([| r7 |], [| r8; r9 |]) 
  (* Other instructions are regular *)
  | _ ->
      super#reload_operation op arg res

end

let fundecl f =
  (new reload)#fundecl f
