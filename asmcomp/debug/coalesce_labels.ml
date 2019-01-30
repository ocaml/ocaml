(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Int = Numbers.Int
module L = Linearize
module Option = Misc.Stdlib.Option

let rewrite_label env label =
  match Int.Map.find label env with
  | exception Not_found -> label
  | label -> label

let rewrite_call_labels env ({ before; after; } : Mach.call_labels)
      : Mach.call_labels =
  { before = rewrite_label env before;
    after = rewrite_label env after;
  }

(* Since there may be both forward references and labels that require
   coalescing (possibly where one of a group to be coalesced was forward
   referenced), we do this in two passes. *)

let rec coalesce env (insn : L.instruction) ~last_insn_was_label =
  match insn.desc with
  | Lend -> env, insn
  | _ ->
    let env, desc, this_insn_is_label =
      match insn.desc with
      | Llabel label ->
        begin match last_insn_was_label with
        | Some rewritten_existing_label ->
          (* This label immediately follows another, so delete it.
             References to it will be rewritten to the previous label. *)
          let env = Int.Map.add label rewritten_existing_label env in
          env, None, last_insn_was_label
        | None ->
          let rewritten_label = Cmm.new_label () in
          let env = Int.Map.add label rewritten_label env in
          env, Some insn.desc, Some rewritten_label
        end
      | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Lpushtrap
      | Lpoptrap | Lraise _ | Lbranch _
      | Lcondbranch _ | Lcondbranch3 _ | Lswitch _ | Lsetuptrap _ ->
        env, Some insn.desc, None
      | Lend -> assert false
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
  match insn.desc with
  | Lend -> insn
  | _ ->
    let desc : L.instruction_desc =
      match insn.desc with
      | Lprologue -> insn.desc
      | Lop op ->
        (* CR mshinwell: This may not actually be needed, but we should
           explain why in the .mli. *)
        let op : Mach.operation =
          match op with
          | Imove
          | Ispill
          | Ireload
          | Iconst_int _
          | Iconst_float _
          | Iconst_symbol _ -> op
          | Icall_ind { call_labels; } ->
            let call_labels = rewrite_call_labels env call_labels in
            Icall_ind { call_labels; }
          | Icall_imm { func; callee_dbg; call_labels; } ->
            let call_labels = rewrite_call_labels env call_labels in
            Icall_imm { func; callee_dbg; call_labels; }
          | Itailcall_ind { call_labels; } ->
            let call_labels = rewrite_call_labels env call_labels in
            Itailcall_ind { call_labels; }
          | Itailcall_imm { func; callee_dbg; call_labels; } ->
            let call_labels = rewrite_call_labels env call_labels in
            Itailcall_imm { func; callee_dbg; call_labels; }
          | Iextcall { func; alloc; call_labels; } ->
            let call_labels = rewrite_call_labels env call_labels in
            Iextcall { func; alloc; call_labels; }
          | Istackoffset _
          | Iload _
          | Istore _ -> op
          | Ialloc { bytes; label_after_call_gc; spacetime_index; } ->
            let label_after_call_gc =
              Option.map (rewrite_label env) label_after_call_gc
            in
            Ialloc { bytes; label_after_call_gc; spacetime_index; }
          | Iintop _
          | Iintop_imm _
          | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
          | Ifloatofint | Iintoffloat
          | Ispecific _
            (* For the moment we assume [Ispecific] operations do not contain
               labels. *)
          | Iname_for_debugger { ident = _; is_parameter = _;
              provenance = _; is_assignment = _; } -> op
        in
        Lop op
      | Lreloadretaddr
      | Lreturn
      | Lpushtrap
      | Lpoptrap
      | Lraise _ -> insn.desc
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
      | Lend -> assert false
    in
    let next = renumber env insn.next in
    { insn with L. desc; next; }

let fundecl (decl : L.fundecl) : int Int.Map.t * L.fundecl =
(*
  Printlinear.fundecl Format.std_formatter decl;
  Format.printf "\n%!";
*)
  let env, fun_body =
    coalesce Int.Map.empty decl.fun_body ~last_insn_was_label:None
  in
  let fun_body = renumber env fun_body in
  let fun_tailrec_entry_point_label =
    rewrite_label env decl.fun_tailrec_entry_point_label
  in
  let decl =
    { decl with
      fun_body;
      fun_tailrec_entry_point_label;
    }
  in
(*
  Printlinear.fundecl Format.std_formatter decl;\
  Format.printf "\n%!";
*)
  env, decl
