(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

let live_at_exit = ref Reg.Set.empty
let live_at_break = ref Reg.Set.empty
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
      (* i.live remains empty since no regs are live across *)
      Reg.set_of_array i.arg
  | Iifthenelse(test, ifso, ifnot) ->
      let at_join = live i.next finally in
      let at_fork = Reg.Set.union (live ifso at_join) (live ifnot at_join) in
      i.live <- at_fork;
      Reg.add_set_array at_fork i.arg
  | Iswitch(index, cases) ->
      let at_join = live i.next finally in
      let at_fork = ref Reg.Set.empty in
      for i = 0 to Array.length cases - 1 do
        at_fork := Reg.Set.union !at_fork (live cases.(i) at_join)
      done;
      i.live <- !at_fork;
      Reg.add_set_array !at_fork i.arg
  | Iloop(body) ->
      let at_top = ref Reg.Set.empty in
      (* Yes, there are better algorithms, but we'll just iterate till
         reaching a fixpoint. *)
      begin try
        while true do
          let new_at_top = Reg.Set.union !at_top (live body !at_top) in
          if Reg.Set.equal !at_top new_at_top then raise Exit;
          at_top := new_at_top
        done
      with Exit -> ()
      end;
      i.live <- !at_top;
      !at_top
  | Icatch(body, handler) ->
      let at_join = live i.next finally in
      let before_handler = live handler at_join in
      let saved_live_at_exit = !live_at_exit in
      live_at_exit := before_handler;
      let before_body = live body at_join in
      live_at_exit := saved_live_at_exit;
      i.live <- before_body;
      before_body
  | Iexit ->
      (* i.live remains empty since no regs are live across *)
      !live_at_exit
  | Itrywith(body, handler) ->
      let at_join = live i.next finally in
      let before_handler = live handler at_join in
      let saved_live_at_raise = !live_at_raise in
      live_at_raise := before_handler;
      let before_body = live body at_join in
      live_at_raise := saved_live_at_raise;
      i.live <- before_body;
      before_body
  | Iraise ->
      (* i.live remains empty since no regs are live across *)
      Reg.add_set_array !live_at_raise i.arg
  | _ ->
      let across = Reg.diff_set_array (live i.next finally) i.res in
      i.live <- across;
      match i.desc with
        Iop(Icall_ind) | Iop(Icall_imm _) | Iop(Iextcall _)->
          (* The function call may raise an exception, branching to the
             nearest enclosing try ... with. Hence, everything that must
             be live at the beginning of the exception handler must also
             be live just before the call. *)
           Reg.add_set_array (Reg.Set.union across !live_at_raise) i.arg
       | _ ->
           Reg.add_set_array across i.arg

let fundecl f =
  live f.fun_body Reg.Set.empty; ()

