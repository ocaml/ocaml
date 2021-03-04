(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Reloading for the ARM 64 bits *)

open Reg

class reload = object (self)

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  match op with
  | Ispecific Imove32 ->
      (* Like Imove: argument or result can be on stack but not both *)
      begin match arg.(0), res.(0) with
      | {loc = Stack s1}, {loc = Stack s2} when s1 <> s2 ->
          ([| self#makereg arg.(0) |], res)
      | _ ->
          (arg, res)
      end
   | _ ->
      super#reload_operation op arg res

end

let fundecl f num_stack_slots =
  (new reload)#fundecl f num_stack_slots
