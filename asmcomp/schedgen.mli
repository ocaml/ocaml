(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Instruction scheduling *)

type code_dag_node =
  { instr: Linearize.instruction;
    delay: int;
    mutable sons: (code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }

class virtual scheduler_generic (unit) =
  (* Can be overriden by processor description *)
  virtual oper_issue_cycles : Mach.operation -> int
      (* Number of cycles needed to issue the given operation *)
  virtual oper_latency : Mach.operation -> int
      (* Number of cycles needed to complete the given operation *)
  method oper_in_basic_block : Mach.operation -> bool
      (* Says whether the given operation terminates a basic block *)

  (* Entry point *)
  method schedule_fundecl : Linearize.fundecl -> Linearize.fundecl
end
