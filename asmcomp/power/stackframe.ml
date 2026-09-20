(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cambium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compute the parameters needed for allocating and managing stack frames
   in the Emit phase. *)

open! Mach [@@warning "-66"]

(* Must match [Emit]'s [trap_size]: 16 bytes normally, larger with frame
   pointers (where [Lpushtrap] allocates a full frame). *)
let trap_handler_size = if Config.with_frame_pointers then 48 else 16

class stackframe = object

inherit Stackframegen.stackframe_generic

method trap_handler_size = trap_handler_size

end

let analyze f =
  (new stackframe)#analyze f
