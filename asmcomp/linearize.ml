(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Transformation of Mach code into a list of pseudo-instructions. *)

open Reg
open Mach

type label = Cmm.label

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t;
    trap_depth: int;
  }

and instruction_desc =
    Lend
  | Lop of operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Ladjust_trap_depth of int
  | Lentertrap
  | Lpushtrap of { handler : label; }
  | Lpoptrap
  | Lraise of Cmm.raise_kind

let has_fallthrough = function
  | Lreturn | Lbranch _ | Lswitch _ | Lraise _
  | Lop Itailcall_ind _ | Lop (Itailcall_imm _) -> false
  | _ -> true

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_spacetime_shape : Mach.spacetime_shape option;
  }

(* Invert a test *)

let invert_integer_test = function
    Isigned cmp -> Isigned(Cmm.negate_comparison cmp)
  | Iunsigned cmp -> Iunsigned(Cmm.negate_comparison cmp)

let invert_test = function
    Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest(cmp) -> Iinttest(invert_integer_test cmp)
  | Iinttest_imm(cmp, n) -> Iinttest_imm(invert_integer_test cmp, n)
  | Ifloattest(cmp, neg) -> Ifloattest(cmp, not neg)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    trap_depth = 0;
  }

(* Add directives to compensate for differences in exception trap depths
   when blocks are juxtaposed. *)

let adjust_trap_depth ~before ~after =
  let adjustment = after.trap_depth - before in
  if adjustment = 0 then
    after
  else
    match after.desc with
    | Ladjust_trap_depth delta ->
      if delta + adjustment = 0 then after.next
      else { after with desc = Ladjust_trap_depth (delta + adjustment);
                        trap_depth = before; }
    | _ ->
      { desc = Ladjust_trap_depth adjustment;
        next = after;
        arg = [||];
        res = [||];
        dbg = Debuginfo.none;
        live = Reg.Set.empty;
        trap_depth = before;
      }

(* Cons an instruction (live, debug empty) assuming that no trap depth
   adjustments need doing.  (Currently only used by [Branch_relaxation].) *)

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty;
    trap_depth = n.trap_depth;
  }

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; live = Reg.Set.empty;
    trap_depth = n.trap_depth;
  }

(* Build an instruction with arg, res, dbg, live taken from
   the given Mach.instruction *)

let copy_instr d i n =
  let trap_depth =
    match d with
    | Lpushtrap _ ->
      assert (n.trap_depth > 0);
      n.trap_depth - 1
    | Lpoptrap -> n.trap_depth + 1
    | _ -> n.trap_depth
  in
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res;
    dbg = i.Mach.dbg; live = i.Mach.live;
    trap_depth;
  }

(*
   Label the beginning of the given instruction sequence.
   - If the sequence starts with a branch, jump over it.
   - If the sequence is the end, (tail call position), just do nothing
*)

let get_label n = match n.desc with
    Lbranch lbl -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | Lend -> (-1, n)
  | _ -> let lbl = Cmm.new_label() in (lbl, cons_instr (Llabel lbl) n)

let get_real_label n =
  match n.desc with
  | Lbranch lbl -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | _ ->
    let lbl = Cmm.new_label () in
    (lbl, cons_instr (Llabel lbl) n)

(* Check the fallthrough label *)
let check_label n = match n.desc with
  | Lbranch lbl -> lbl
  | Llabel lbl -> lbl
  | _ -> -1

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating
   instruction. *)

let rec discard_dead_code n =
  match n.desc with
    Lend -> n
  | Llabel _ -> n
    (* Do not discard Lpoptrap/Lpushtrap or Istackoffset instructions,
       as this may cause a stack imbalance later during assembler generation.
       However it's ok to eliminate dead instructions after them. *)
  | Lpoptrap | Lpushtrap _
  | Lop(Istackoffset _) -> { n with next = discard_dead_code n.next; }
  | Ladjust_trap_depth _ ->
    let before = n.trap_depth in
    let after = discard_dead_code n.next in
    adjust_trap_depth ~before ~after
  | _ -> discard_dead_code n.next

(*
   Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through
   or if we jump to dead code after the end of function (lbl=-1)
*)

let add_branch lbl n =
  if lbl >= 0 then
    let n1 = discard_dead_code n in
    match n1.desc with
    | Llabel lbl1 when lbl1 = lbl -> n1
    | _ -> cons_instr (Lbranch lbl) n1
  else
    discard_dead_code n

(* Association list: exit handler -> (handler label, trap depth) *)

let exit_label = ref []

let find_exit_label_trap_depth k =
  try
    List.assoc k !exit_label
  with
  | Not_found -> Misc.fatal_error "Linearize.find_exit_label"

let find_exit_label k =
  let (label, _trap_depth') = find_exit_label_trap_depth k in
  label

let is_next_catch n ~trap_depth =
  match !exit_label with
  | (n0,(_,t))::_  when n0=n && t = trap_depth -> true
  | _ -> false

(* Linearize an instruction [i]: add it in front of the continuation [n] *)

let rec linear i n =
  match i.Mach.desc with
    Iend -> n
  | Iop(Itailcall_ind _ | Itailcall_imm _ as op) ->
      let n = adjust_trap_depth ~before:0 ~after:n in
      if not Config.spacetime then
        copy_instr (Lop op) i (discard_dead_code n)
      else
        copy_instr (Lop op) i (linear i.Mach.next n)
  | Iop(Imove | Ireload | Ispill)
    when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
      linear i.Mach.next n
  | Iop op ->
      copy_instr (Lop op) i (linear i.Mach.next n)
  | Ireturn ->
      let n = adjust_trap_depth ~before:0 ~after:n in
      let n1 = copy_instr Lreturn i (discard_dead_code n) in
      if !Proc.contains_calls
      then cons_instr Lreloadretaddr n1
      else n1
  | Iifthenelse(test, ifso, ifnot) ->
      let n1 = linear i.Mach.next n in
      let trap_depth = n1.trap_depth in
      begin match (ifso.Mach.desc, ifnot.Mach.desc, n1.desc) with
        Iend, _, Lbranch lbl ->
          copy_instr (Lcondbranch(test, lbl)) i (linear ifnot n1)
      | _, Iend, Lbranch lbl ->
          copy_instr (Lcondbranch(invert_test test, lbl)) i (linear ifso n1)
      | Iexit (nfail1, Clambda.No_action), Iexit (nfail2, Clambda.No_action), _
            when is_next_catch nfail1 ~trap_depth ->
          let lbl2 = find_exit_label nfail2 in
          copy_instr
            (Lcondbranch (invert_test test, lbl2)) i (linear ifso n1)
      | Iexit (nfail, Clambda.No_action), _, _ ->
          let n2 = linear ifnot n1
          and lbl = find_exit_label nfail in
          copy_instr (Lcondbranch(test, lbl)) i n2
      | _,  Iexit (nfail, Clambda.No_action), _ ->
          let n2 = linear ifso n1 in
          let lbl = find_exit_label nfail in
          copy_instr (Lcondbranch(invert_test test, lbl)) i n2
      | Iend, _, _ ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(test, lbl_end)) i (linear ifnot n2)
      | _,  Iend, _ ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(invert_test test, lbl_end)) i
                     (linear ifso n2)
      | _, _, _ ->
        (* Should attempt branch prediction here *)
          let (lbl_end, n2) = get_label n1 in
          let (lbl_else, nelse) = get_label (linear ifnot n2) in
          copy_instr (Lcondbranch(invert_test test, lbl_else)) i
            (linear ifso (add_branch lbl_end nelse))
      end
  | Iswitch(index, cases) ->
      let lbl_cases = Array.make (Array.length cases) 0 in
      let (lbl_end, n1) = get_label(linear i.Mach.next n) in
      let n2 = ref (discard_dead_code n1) in
      for i = Array.length cases - 1 downto 0 do
        let (lbl_case, ncase) =
                get_label(linear cases.(i) (add_branch lbl_end !n2)) in
        lbl_cases.(i) <- lbl_case;
        n2 := discard_dead_code ncase
      done;
      (* Switches with 1 and 2 branches have been eliminated earlier.
         Here, we do something for switches with 3 branches. *)
      if Array.length index = 3 then begin
        let fallthrough_lbl = check_label !n2 in
        let find_label n =
          let lbl = lbl_cases.(index.(n)) in
          if lbl = fallthrough_lbl then None else Some lbl in
        copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                   i !n2
      end else
        copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
  | Iloop body ->
      let lbl_head = Cmm.new_label() in
      let n1 = linear i.Mach.next n in
      let n2 = linear body (cons_instr (Lbranch lbl_head) n1) in
      cons_instr (Llabel lbl_head) n2
  | Icatch(_rec_flag, is_exn_handler, handlers, body) ->
      (* CR mshinwell: There were two problems:
         1. The patch to introduce multiple handlers had forgotten to put
            a branch at the end of each handler to the join point.
         2. For:
              catch
                catch
                  ...
                with k1 -> ...
              with k2 -> ...
              Iend
            then the "lbl_end" for the outermost catch is -1.  This means that
            the recursive call to linearize the body of the catch would never
            add a branch before the handler k2, since the code before that
            branch must be unreachable.  So when we come to the second Icatch
            then "lbl_end" ends up getting the label from the start of [k2],
            which can cause dead "goto"s to jump to exception handling
            continuations.  (Causes [Linear_invariants] to fail.)
            It also seems as if the dead code elimination won't correctly
            propagate backwards at present due to this -1 being lost.  It's
            unclear to me whether there is any solution other than passing
            [lbl_end] to [linear].
            For the moment we don't use this -1 label at all, and make sure
            there's a real label there.
            Maybe there should be a separate pass for doing the dead code
            elimination on Linear?
      *)
      (* CR mshinwell: Change the type to make this statically checked *)
      assert (not is_exn_handler || List.length handlers = 1);
      let (lbl_end, n1) = get_real_label (linear i.Mach.next n) in
      (* CR mshinwell for pchambart:
         1. rename "io"
         2. Make sure the test cases cover the "Iend" cases too *)
      let labels_at_entry_to_handlers = List.map (fun (_nfail, _, handler) ->
          match handler.Mach.desc with
          | Iend -> lbl_end
          | _ -> Cmm.new_label ())
          handlers in
      let previous_exit_label = !exit_label in
      let exit_label_add =
        List.map2 (fun (cont, trap_stack, _) lbl ->
            (cont, (lbl, List.length trap_stack)))
          handlers labels_at_entry_to_handlers
      in
      exit_label := exit_label_add @ !exit_label;
      let n2 =
        List.fold_left2 (fun n (_nfail, trap_stack, handler) lbl_handler ->
            match handler.Mach.desc with
            | Iend -> n
            | _ ->
              let trap_depth = List.length trap_stack in
              let n = adjust_trap_depth ~before:trap_depth ~after:n in
              let handler = linear handler (add_branch lbl_end n) in
              let handler =
                if is_exn_handler then
                  cons_instr Lentertrap handler
                else
                  handler
              in
              cons_instr (Llabel lbl_handler) handler)
          n1 handlers labels_at_entry_to_handlers
      in
      let n2 = adjust_trap_depth ~before:n1.trap_depth ~after:n2 in
      let n3 = linear body (add_branch lbl_end n2) in
      exit_label := previous_exit_label;
      n3
  | Iexit (nfail, ta) ->
      let lbl, trap_depth = find_exit_label_trap_depth nfail in
      let n1 = linear i.Mach.next n in
      let n2 = adjust_trap_depth ~before:trap_depth ~after:n1 in
      let n3 = add_branch lbl n2 in
      begin match ta with
      | Clambda.No_action -> n3
      | Clambda.Pop cl ->
          let add_poptrap _ n =
            { (cons_instr Lpoptrap n) with trap_depth = n.trap_depth + 1 }
          in
          List.fold_right add_poptrap cl n3
      | Clambda.Push cl ->
        let add_pushtrap cont n =
          assert (n.trap_depth > 0);
          let handler = find_exit_label cont in
          { (cons_instr (Lpushtrap { handler; }) n)
            with trap_depth = n.trap_depth - 1; }
        in
        List.fold_right add_pushtrap cl n3
      end
  | Iraise (k, trap_stack) ->
      let trap_depth = List.length trap_stack in
      let n = adjust_trap_depth ~before:trap_depth ~after:n in
      copy_instr (Lraise k) i (discard_dead_code n)

let fundecl f =
  exit_label := [];
  { fun_name = f.Mach.fun_name;
    fun_body = linear f.Mach.fun_body end_instr;
    fun_fast = f.Mach.fun_fast;
    fun_dbg  = f.Mach.fun_dbg;
    fun_spacetime_shape = f.Mach.fun_spacetime_shape;
  }
