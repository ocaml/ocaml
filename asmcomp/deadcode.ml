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

module IntSet = Set.Make(
  struct
    type t = int
    let compare (x:t) y = compare x y
  end)

type d = {
  i : instruction;   (* optimized instruction *)
  regs : Reg.Set.t;  (* a set of registers live "before" instruction [i] *)
  exits : IntSet.t;  (* indexes of Iexit instructions "live before" [i] *)
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
    { i; regs; exits = IntSet.empty; }
  | Iop op ->
      let s = deadcode i.next in
      if Proc.op_is_pure op                     (* no side effects *)
      && Reg.disjoint_set_array s.regs i.res (* results are not used after *)
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
        exits = IntSet.union s.exits
                  (IntSet.union ifso'.exits ifnot'.exits);
      }
  | Iswitch(index, cases) ->
      let dc = Array.map deadcode cases in
      let cases' = Array.map (fun c -> c.i) dc in
      let s = deadcode i.next in
      { i = {i with desc = Iswitch(index, cases'); next = s.i};
        regs = Reg.add_set_array i.live arg;
        exits = Array.fold_left
                  (fun acc c -> IntSet.union acc c.exits) s.exits dc;
      }
  | Icatch(rec_flag, handlers, body) ->
    let body' = deadcode body in
    let s = deadcode i.next in
    let (handlers', h_exits) =
      List.split
        (List.map (fun (nfail, handler) ->
           let handler' = deadcode handler in
           ((nfail, handler'.i), handler'.exits))
           handlers)
    in
    (* Remove unused handlers.
       We deal with three disjoint sets of "nfail" indexes:
       dead handler indexes = B \setminus A
       used handler indexes = A \intersect B
       live exit indexes = A \setminus B
       where
       B = indexes of handlers in this Icatch,
       A = indexes used in Iexit instructions that are
       in scope of the handlers of this Icatch (scope depends on rec_flag).
    *)
    let h_exits = List.fold_left (fun acc e -> IntSet.union acc e)
                    IntSet.empty h_exits in
    let all_exits = IntSet.union body'.exits h_exits in
    let exits_in_scope =
      match rec_flag with
      | Cmm.Recursive -> all_exits
        (* Indexes mentioned in Iexit instructions in body and handlers. *)
      | Cmm.Nonrecursive -> body'.exits
        (* Icatch body is allowed to Iexit to it's own hanlders
           or handlers of an enclosing Icatch that are not shadowed
           by the current Icatch.
           A handler cannot Iexit to itself or other handlers in this Icatch.
           Thus, all Iexits in handlers refer to an enclosing Icatch. *)
    in
    (* A handler is "used" if and only if its index is in exits_in_scope .*)
    let used_handlers =
      List.filter (fun (n,_)-> IntSet.mem n exits_in_scope) handlers' in
    let used_handler_indexes =
      IntSet.of_list (fst (List.split used_handlers)) in
    (* Exits that do not have a used handler with the same index
       are referring to a handler in an enclosing catch.
       They are added to "live before" exits of instruction [i] *)
    let live_exits = IntSet.diff all_exits used_handler_indexes in
    { i = {i with desc = Icatch(rec_flag, used_handlers, body'.i); next = s.i};
      regs = i.live;
      exits = IntSet.union s.exits live_exits;
    }
  | Iexit nfail ->
      { i;  regs = i.live; exits = IntSet.singleton nfail; }
  | Itrywith(body, handler) ->
      let body' = deadcode body in
      let handler' = deadcode handler in
      let s = deadcode i.next in
      { i = {i with desc = Itrywith(body'.i, handler'.i); next = s.i};
        regs = i.live;
        exits = IntSet.union s.exits
                  (IntSet.union body'.exits handler'.exits);
      }

let fundecl f =
  let new_body = deadcode f.fun_body in
  {f with fun_body = new_body.i}
