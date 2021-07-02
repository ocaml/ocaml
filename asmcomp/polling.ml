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

(* We use a backwards dataflow analysis to compute a single value: either
   "Might_not_poll" or "Always_polls".

   "Might_not_poll" means there exists a path from the function entry to a
   Potentially Recursive Tail Call (an Itailcall_ind or
   Itailcall_imm to a forward function)
   that does not go through an Ialloc or Ipoll instruction.

   "Always_polls", therefore, means the function always polls (via Ialloc or
   Ipoll) before doing a PRTC.
*)

type polls_before_prtc = Might_not_poll | Always_polls

module Polls_before_prtc = struct
  type t = polls_before_prtc

  let bot = Always_polls

  let join t1 t2 =
    match t1, t2 with
    | Might_not_poll, Might_not_poll
    | Might_not_poll, Always_polls
    | Always_polls, Might_not_poll -> Might_not_poll
    | Always_polls, Always_polls -> Always_polls

  let lessequal t1 t2 =
    match t1, t2 with
    | Always_polls, Always_polls
    | Always_polls, Might_not_poll
    | Might_not_poll, Might_not_poll -> true
    | Might_not_poll, Always_polls -> false
end

module PTRCAnalysis = Dataflow.Backward(Polls_before_prtc)

let potentially_recursive_tailcall ~fwd_func funbody =
  let transfer i ~next ~exn =
    match i.desc with
    | Iend -> next
    | Iop (Ialloc _ | Ipoll _) -> Always_polls
    | Iop (Itailcall_ind) -> Might_not_poll
    | Iop (Itailcall_imm { func }) ->
      (* We optimise by making a partial ordering over Mach functions: in
         definition order within a compilation unit, and dependency order
         between compilation units. This order is acyclic, as OCaml does not
         allow circular dependencies between modules.  It's also finite, so if
         there's an infinite sequence of function calls then something has to
         make a forward reference.

         Also, in such an infinite sequence of function calls, at most finitely
         many of them can be non-tail calls. (If there are infinitely many
         non-tail calls, then the program soon terminates with a stack
         overflow).

         So, every such infinite sequence must contain many forward-referencing
         tail calls, so polling only on those suffices.  This is checked using
         the set [fwd_func]. *)
      if String.Set.mem func fwd_func
         || is_assume_suppressed_poll_fun func
      then Might_not_poll
      else Always_polls
    | Iop op ->
        if operation_can_raise op
        then Polls_before_prtc.join next exn
        else next
    | Ireturn -> Always_polls
    | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _ | Itrywith _ -> next
    | Iraise _ -> exn
  in
  fst (PTRCAnalysis.analyze ~transfer funbody)

(* Given the result of the analysis of recursive handlers,
   add polls to make sure that every loop polls or allocate or ...

   [Ipoll] instructions are added before unguarded back edges
   ([Iexit] instructions that go back to a recursive handler that
   needs extra polling).
*)

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
        if rc = Cmm.Recursive
        then List.fold_left add_unsafe_handler ube hdl
        else ube in
      let instr_handler (k, i0) =
        let i1 = instr ube' i0 in
        (k, i1) in
      { i with
        desc = Icatch (rc,
                       List.map instr_handler hdl,
                       instr ube body);
        next = instr ube i.next;
      }
    | Iexit k ->
        if IntSet.mem k ube
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

let contains_poll instr =
  let poll = ref false in
  Mach.instr_iter
      (fun i -> match i.desc with Iop (Ipoll _) -> poll := true | _ -> ())
      instr;
  !poll

let instrument_fundecl ~future_funcnames:_ (f : Mach.fundecl) : Mach.fundecl =
  let handler_needs_poll = polled_loops_analysis f.fun_body in
  let new_body = instr_body handler_needs_poll f.fun_body in
  let new_contains_calls = f.fun_contains_calls || contains_poll new_body in
  { f with fun_body = new_body; fun_contains_calls = new_contains_calls }

let requires_prologue_poll ~future_funcnames i =
  match potentially_recursive_tailcall ~fwd_func:future_funcnames i with
  | Might_not_poll -> true
  | Always_polls -> false
