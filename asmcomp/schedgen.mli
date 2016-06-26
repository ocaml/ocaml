(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction scheduling *)

module Make (Arch : Arch_intf.S) (Proc : Proc_intf.S with type addressing_mode = Arch.addressing_mode and type specific_operation = Arch.specific_operation) : sig
type code_dag_node =
  { instr: (Arch.addressing_mode, Arch.specific_operation) Linearize.instruction;
    delay: int;
    mutable sons: (code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }

class virtual scheduler_generic : object
  (* Can be overridden by processor description *)
  method virtual oper_issue_cycles : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> int
      (* Number of cycles needed to issue the given operation *)
  method virtual oper_latency : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> int
      (* Number of cycles needed to complete the given operation *)
  method reload_retaddr_issue_cycles : int
      (* Number of cycles needed to issue a Lreloadretaddr operation *)
  method reload_retaddr_latency : int
      (* Number of cycles needed to complete a Lreloadretaddr operation *)
  method oper_in_basic_block : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> bool
      (* Says whether the given operation terminates a basic block *)
  method is_store : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a memory store *)
  method is_load : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a memory load *)
  method is_checkbound : (Arch.addressing_mode, Arch.specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a checkbound *)
  (* Entry point *)
  method schedule_fundecl : (Arch.addressing_mode, Arch.specific_operation) Linearize.fundecl -> (Arch.addressing_mode, Arch.specific_operation) Linearize.fundecl
end

val reset : unit -> unit
end
