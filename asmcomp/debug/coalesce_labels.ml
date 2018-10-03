(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int = Numbers.Int
module L = Linearize
module Option = Misc.Stdlib.Option

let rewrite_label env label =
  match Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

(* Since there may be both forward references and labels that require
   coalescing (possibly where one of a group to be coalesced was forward
   referenced), we do this in two passes. *)

let rec coalesce env (insn : L.instruction) ~last_insn_was_label =
  if insn == L.end_instr then
    env, insn
  else
    let env, desc, this_insn_is_label =
      match insn.desc with
      | Llabel label ->
        begin match last_insn_was_label with
        | Some existing_label ->
          (* This label immediately follows another, so delete it.
             References to it will be rewritten to the previous label. *)
          let env = Int.Map.add label existing_label env in
          env, None, last_insn_was_label
        | None ->
          env, Some insn.desc, Some label
        end
      (* CR mshinwell: Figure out why the following leaves gaps in location
         lists... sometimes *)
      | Lcapture_stack_offset _ (*->
        (* This is effectively a label, and doesn't generate any code.
           As such, allow labels to be coalesced even if it is in the
           middle. *)
        env, Some insn.desc, last_insn_was_label *)
      | Lprologue | Lend | Lop _ | Lreloadretaddr | Lreturn | Lpushtrap
      | Lpoptrap | Lraise _ | Lbranch _
      | Lcondbranch _ | Lcondbranch3 _ | Lswitch _ | Lsetuptrap _ ->
        env, Some insn.desc, None
    in
    let env, next =
      coalesce env insn.next ~last_insn_was_label:this_insn_is_label
    in
    let insn =
      match desc with
      | None -> next
      | Some desc ->
        { insn with
          desc;
          next;
        }
    in
    env, insn

let rec renumber env (insn : L.instruction) =
  if insn == L.end_instr then
    insn
  else
    let desc : L.instruction_desc =
      match insn.desc with
      | Lprologue
      | Lend
      | Lop _
      | Lreloadretaddr
      | Lreturn
      | Lpushtrap
      | Lpoptrap
      | Lraise _
      | Lcapture_stack_offset _ -> insn.desc
      | Llabel label -> Llabel (rewrite_label env label)
      | Lbranch label -> Lbranch (rewrite_label env label)
      | Lcondbranch (test, label) ->
        Lcondbranch (test, rewrite_label env label)
      | Lcondbranch3 (label1_opt, label2_opt, label3_opt) ->
        Lcondbranch3 (
          Option.map (rewrite_label env) label1_opt,
          Option.map (rewrite_label env) label2_opt,
          Option.map (rewrite_label env) label3_opt)
      | Lswitch labels ->
        Lswitch (Array.map (rewrite_label env) labels)
      | Lsetuptrap label -> Lsetuptrap (rewrite_label env label)
    in
    let next = renumber env insn.next in
    { insn with L. desc; next; }

let fundecl (decl : L.fundecl) : int Int.Map.t * L.fundecl =
  if not !Clflags.debug then Int.Map.empty, decl
  else begin
    let env, fun_body =
      coalesce Int.Map.empty decl.fun_body ~last_insn_was_label:None
    in
    let fun_body = renumber env fun_body in
    let decl =
      { decl with
        fun_body;
      }
    in
    env, decl
  end
