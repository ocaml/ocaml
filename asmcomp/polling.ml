(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*        Xavier Leroy and Damien Doligez, projet Cambium, INRIA Paris    *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Mach

module String = Misc.Stdlib.String
module IntSet = Set.Make(Int)

(* replace with starts_with when it arrives *)
let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let is_assume_suppressed_poll_fun s =
  isprefix "caml_apply" s

(* Detection of recursive handlers that are not guaranteed to poll
   at every loop iteration. *)

(* The result of the analysis is a mapping from handlers H
   (= loop heads) to Booleans b.

   b is true if every path starting from H goes through an Ialloc,
   Ipoll, Ireturn, Itailcall_ind or Itailcall_imm instruction.
   In this case, we say that H is "safe".

   b is false, therefore, if starting from H we can loop infinitely
   without crossing an Ialloc or Ipoll instruction.
   In this case, we say that H is "unsafe".
*)

(* The analysis is a backward dataflow analysis starting from false,
   using && (Boolean "and") as the join operator,
   and with the following transfer function:

   TRANSF(Ialloc | Ipoll | Itailcall_ind | Itailcall_imm _ | Ireturn) = true
   TRANSF(all other operations, x) = x
*)

module PolledLoopsAnalysis = Dataflow.Backward(struct
  type t = bool
  let bot = false
  let join = (&&)
  let lessequal a b = (not a) || b
end)

let polled_loops_analysis funbody =
  let transfer i ~next ~exn =
    match i.desc with
    | Iend -> next
    | Iop (Ialloc _ | Ipoll _) -> true
    | Iop (Itailcall_ind | Itailcall_imm _) -> true
    | Iop op ->
        if operation_can_raise op
        then next && exn
        else next
    | Ireturn -> true
    | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ | Itrywith _ -> next
    | Iraise _ -> exn
  in
    snd (PolledLoopsAnalysis.analyze ~exnescape:true ~transfer funbody)

(* Detection of functions that can loop via a tail-call without going
   through a poll point. *)

(* The result of the analysis is a single Boolean b.

   b is true if there exists a path from the function entry to a
   Potentially Recursive Tail Call (an Itailcall_ind or
   Itailcall_imm to a forward function)
   that does not go through an Ialloc or Ipoll instruction.

   b is false, therefore, if the function always polls (via Ialloc or Ipoll)
   before doing a PRTC.

   To compute b, we do a backward dataflow analysis starting from
   false, using || (Boolean "or") as the join operator, and with the
   following transfer function:

   TRANSF(Ialloc | Ipoll, x) = false
   TRANSF(Itailcall_ind, x) = true
   TRANSF(Itailcall_imm f, x) = f is a forward function
   TRANSF(all other operations, x) = x
*)

module PTRCAnalysis = Dataflow.Backward(struct
  type t = bool
  let bot = false
  let join = (||)
  let lessequal a b = (not a) || b
end)

let potentially_recursive_tailcall ~fwd_func funbody =
  let transfer i ~next ~exn =
    match i.desc with
    | Iend -> next
    | Iop (Ialloc _ | Ipoll _) -> false
    | Iop (Itailcall_ind) -> true
    | Iop (Itailcall_imm { func }) ->
      String.Set.mem func fwd_func ||
      is_assume_suppressed_poll_fun func
    | Iop op ->
        if operation_can_raise op
        then next || exn
        else next
    | Ireturn -> false
    | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ | Itrywith _ -> next
    | Iraise _ -> exn
  in
    fst (PTRCAnalysis.analyze ~transfer funbody)

(* Given the result of the analysis of recursive handlers,
   add polls to make sure that every loop polls or allocate or ...
   Two strategies are supported for adding polls:
 - if [poll_location] is [At_top], [Ipoll] instructions are added to
   the beginning of every recursive handler that needs extra polling;
 - if [poll_location] is [At_bottom], [Ipoll] instructions are added
   before unguarded back edges ([Iexit] instructions that go back
   to a recursive handler that needs extra polling).
*)

type poll_location = At_top | At_bottom
let poll_location = At_bottom

let add_poll i =
  Mach.instr_cons (Iop (Ipoll { return_label = None })) [||] [||] i

let instr_body handler_safe i =
  (* [ube] (unguarded back edges) is the set of recursive handler labels
     that need extra polling. *)
  let add_unsafe_handler ube (k, _) =
    if handler_safe k then ube else IntSet.add k ube in
  let rec instr ube i =
    match i.desc with
    | Iifthenelse (test, i0, i1) ->
      { i with
        desc = Iifthenelse (test, instr ube i0, instr ube i1);
        next = instr ube i.next;
      }
    | Iswitch (index, cases) ->
      { i with
        desc = Iswitch (index, Array.map (instr ube) cases);
        next = instr ube i.next;
      }
    | Icatch (rc, hdl, body) ->
      let ube' =
        if rc = Cmm.Recursive && poll_location = At_bottom
        then List.fold_left add_unsafe_handler ube hdl
        else ube in
      let instr_handler (k, i0) =
        let i1 = instr ube' i0 in
        if rc = Cmm.Recursive && poll_location = At_top && not (handler_safe k)
        then (k, add_poll i1)
        else (k, i1) in
      { i with
        desc = Icatch (rc,
                       List.map instr_handler hdl,
                       instr ube body);
        next = instr ube i.next;
      }
    | Iexit k ->
        if poll_location = At_bottom && IntSet.mem k ube
        then add_poll i
        else i
    | Itrywith (body, hdl) ->
      { i with
        desc = Itrywith (instr ube body, instr ube hdl);
        next = instr ube i.next;
      }
    | Iend | Ireturn | Iraise _ -> i
    | Iop _ -> { i with next = instr ube i.next }

  in
  instr IntSet.empty i

let instrument_fundecl ~future_funcnames:_ (f : Mach.fundecl) : Mach.fundecl =
  let handler_needs_poll = polled_loops_analysis f.fun_body in
  { f with fun_body = instr_body handler_needs_poll f.fun_body }

let requires_prologue_poll ~future_funcnames i =
  potentially_recursive_tailcall ~fwd_func:future_funcnames i
