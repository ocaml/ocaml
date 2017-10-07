(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Arch
open Mach

(* Reloading for the ARM *)

class reload = object

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  match op with
  | Iintop Imul | Ispecific Imuladd ->
      (* On ARM v4 and v5, module [Selection] adds a second, dummy
         result to multiplication instructions (mul and muladd).  This
         second result is the same pseudoregister as the first
         argument to the multiplication.  As shown in MPR#7642,
         reloading must maintain this invariant.  Otherwise, the second
         result and the first argument can end up in different registers,
         and the second result can be used later, even though
         it is not initialized. *)
      let ((arg', res') as argres') = super#reload_operation op arg res in
      if Array.length res' >= 2 then res'.(1) <- arg'.(0);
      argres'
  | _ ->
      super#reload_operation op arg res

end

let fundecl f =
  (new reload)#fundecl f
