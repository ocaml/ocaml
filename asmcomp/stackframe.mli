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

val trap_handler_size : int

val analyze : Mach.fundecl -> bool * bool * int
  (** [analyze f] returns a triple
      [(contains_nontail_calls, frame_required, extra_space)].

      [contains_nontail_calls] says whether the function contains
      non-tail calls to OCaml functions.  Calls to C functions don't count.

      [frame_required] says whether the function must allocate a stack
      frame on entry, for one of the following reasons:
        - some local variables are stack-allocated
        - the function needs to save its return address on the stack, e.g.:
           -- it contains a non-tail call to an OCaml function
           -- it calls a C function
           -- it contains an allocation or a poll point
           -- it performs an array bound check (on some ports)

      [extra_stack_used] is the mount of stack space used, in bytes,
      in addition to the initial stack frame.
      This counts trap handlers and "outgoing" stack slots
      used for parameter passing. *)
