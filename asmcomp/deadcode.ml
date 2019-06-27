(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

open Mach

module Int = Identifiable.Make (Numbers.Int)

type d = {
  i : instruction;   (* optimized instruction *)
  regs : Reg.Set.t;  (* a set of registers live "before" instruction [i] *)
  exits : Int.Set.t;  (* indexes of Iexit instructions "live before" [i] *)
}

let rec deadcode i =
  let arg =
    if Config.spacetime
      && Mach.spacetime_node_hole_pointer_is_live_before i
    then Array.append i.arg [| Proc.loc_spacetime_node_hole |]
    else i.arg
  in
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) | Iraise _ ->
      let regs = Reg.add_set_array i.live arg in
      { i; regs; exits = Int.Set.empty; }
  | Iop op ->
      let s = deadcode i.next in
      if Proc.op_is_pure op                     (* no side effects *)
      && Reg.disjoint_set_array s.regs i.res   (* results are not used after *)
      && not (Proc.regs_are_volatile arg)      (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        assert (Array.length i.res > 0);  (* sanity check *)
        s
      end else begin
        { i = {i with next = s.i};
          regs = Reg.add_set_array i.live arg;
          exits = s.exits;
        }
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let ifso' = deadcode ifso in
      let ifnot' = deadcode ifnot in
      let s = deadcode i.next in
      { i = {i with desc = Iifthenelse(test, ifso'.i, ifnot'.i); next = s.i};
        regs = Reg.add_set_array i.live arg;
        exits = Int.Set.union s.exits
                  (Int.Set.union ifso'.exits ifnot'.exits);
      }
  | Iswitch(index, cases) ->
      let dc = Array.map deadcode cases in
      let cases' = Array.map (fun c -> c.i) dc in
      let s = deadcode i.next in
      { i = {i with desc = Iswitch(index, cases'); next = s.i};
        regs = Reg.add_set_array i.live arg;
        exits = Array.fold_left
                  (fun acc c -> Int.Set.union acc c.exits) s.exits dc;
      }
  | Icatch(rec_flag, handlers, body) ->
    let body' = deadcode body in
    let s = deadcode i.next in
    let handlers' = Int.Map.map deadcode (Int.Map.of_list handlers) in
    let rec add_live exit (live_exits, used_handlers) =
      if Int.Set.mem exit live_exits then
        (live_exits, used_handlers)
      else
        let live_exits = Int.Set.add exit live_exits in
        match Int.Map.find_opt exit handlers' with
        | None -> (live_exits, used_handlers)
        | Some handler ->
            Int.Set.fold add_live handler.exits
              (live_exits, (exit, handler.i) :: used_handlers)
    in
    let live_exits, used_handlers =
      Int.Set.fold add_live body'.exits (s.exits, [])
    in
    { i = {i with desc = Icatch(rec_flag, used_handlers, body'.i); next = s.i};
      regs = i.live;
      exits = live_exits;
    }
  | Iexit nfail ->
      { i;  regs = i.live; exits = Int.Set.singleton nfail; }
  | Itrywith(body, handler) ->
      let body' = deadcode body in
      let handler' = deadcode handler in
      let s = deadcode i.next in
      { i = {i with desc = Itrywith(body'.i, handler'.i); next = s.i};
        regs = i.live;
        exits = Int.Set.union s.exits
                  (Int.Set.union body'.exits handler'.exits);
      }

let fundecl f =
  let new_body = deadcode f.fun_body in
  {f with fun_body = new_body.i}
