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

class virtual stackframe_generic : object
  method virtual trap_handler_size : int
  method is_call : Mach.instruction_desc -> bool
  method frame_required : Mach.fundecl -> bool -> bool
  method analyze : Mach.fundecl -> bool * bool * int
end

