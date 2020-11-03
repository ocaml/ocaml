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

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

let live_at_exit = ref []

let find_live_at_exit k =
  try
    List.assoc k !live_at_exit
  with
  | Not_found -> Misc.fatal_error "Liveness.find_live_at_exit"

let live_at_raise = ref Reg.Set.empty

let rec live i finally =
  (* finally is the set of registers live after execution of the
     instruction sequence.
     The result of the function is the set of registers live just
     before the instruction sequence.
     The instruction i is annotated by the set of registers live across
     the instruction. *)
  match i.desc with
    Iend ->
      i.live <- finally;
      finally
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      i.live <- Reg.Set.empty; (* no regs are live across *)
      Reg.set_of_array i.arg
  | Iop op ->
      let after = live i.next finally in
      if Proc.op_is_pure op                    (* no side effects *)
      && Reg.disjoint_set_array after i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile i.arg)    (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        (* This operation is dead code.  Ignore its arguments. *)
        i.live <- after;
        after
      end else begin
        let across_after = Reg.diff_set_array after i.res in
        let across =
          match op with
          | Icall_ind | Icall_imm _ | Iextcall _ | Ialloc _
          | Iintop (Icheckbound) | Iintop_imm(Icheckbound, _) ->
              (* The function call may raise an exception, branching to the
                 nearest enclosing try ... with. Similarly for bounds checks
                 and allocation (for the latter: finalizers may throw
                 exceptions, as may signal handlers).
                 Hence, everything that must be live at the beginning of
                 the exception handler must also be live across this instr. *)
               Reg.Set.union across_after !live_at_raise
           | _ ->
               across_after in
        i.live <- across;
        Reg.add_set_array across i.arg
      end
  | Iifthenelse(_test, ifso, ifnot) ->
      let at_join = live i.next finally in
      let at_fork = Reg.Set.union (live ifso at_join) (live ifnot at_join) in
      i.live <- at_fork;
      Reg.add_set_array at_fork i.arg
  | Iswitch(_index, cases) ->
      let at_join = live i.next finally in
      let at_fork = ref Reg.Set.empty in
      for i = 0 to Array.length cases - 1 do
        at_fork := Reg.Set.union !at_fork (live cases.(i) at_join)
      done;
      i.live <- !at_fork;
      Reg.add_set_array !at_fork i.arg
  | Icatch(rec_flag, handlers, body) ->
      let at_join = live i.next finally in
      let aux (nfail,handler) (nfail', before_handler) =
        assert(nfail = nfail');
        let before_handler' = live handler at_join in
        nfail, Reg.Set.union before_handler before_handler'
      in
      let aux_equal (nfail, before_handler) (nfail', before_handler') =
        assert(nfail = nfail');
        Reg.Set.equal before_handler before_handler'
      in
      let live_at_exit_before = !live_at_exit in
      let rec fixpoint before_handlers =
        live_at_exit := before_handlers @ !live_at_exit;
        let before_handlers' = List.map2 aux handlers before_handlers in
        live_at_exit := live_at_exit_before;
        match rec_flag with
        | Cmm.Nonrecursive ->
            before_handlers'
        | Cmm.Recursive ->
            if List.for_all2 aux_equal before_handlers before_handlers'
            then before_handlers'
            else fixpoint before_handlers'
      in
      let init_state =
        List.map (fun (nfail, _handler) -> nfail, Reg.Set.empty) handlers
      in
      let before_handler = fixpoint init_state in
      (* We could use handler.live instead of Reg.Set.empty as the initial
         value but we would need to clean the live field before doing the
         analysis (to remove remnants of previous passes). *)
      live_at_exit := before_handler @ !live_at_exit;
      let before_body = live body at_join in
      live_at_exit := live_at_exit_before;
      i.live <- before_body;
      before_body
  | Iexit nfail ->
      let this_live = find_live_at_exit nfail in
      i.live <- this_live ;
      this_live
  | Itrywith(body, handler) ->
      let at_join = live i.next finally in
      let before_handler = live handler at_join in
      let saved_live_at_raise = !live_at_raise in
      live_at_raise := Reg.Set.remove Proc.loc_exn_bucket before_handler;
      let before_body = live body at_join in
      live_at_raise := saved_live_at_raise;
      i.live <- before_body;
      before_body
  | Iraise _ ->
      i.live <- !live_at_raise;
      Reg.add_set_array !live_at_raise i.arg

let reset () =
  live_at_raise := Reg.Set.empty;
  live_at_exit := []

let fundecl f =
  let initially_live = live f.fun_body Reg.Set.empty in
  (* Sanity check: only function parameters can be live at entrypoint *)
  let wrong_live = Reg.Set.diff initially_live (Reg.set_of_array f.fun_args) in
  if not (Reg.Set.is_empty wrong_live) then begin
    Misc.fatal_errorf "@[Liveness.fundecl:@\n%a@]"
      Printmach.regset wrong_live
  end
