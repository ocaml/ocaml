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

module RD = Reg_with_debug_info

type t =
  | Unreachable
  | Ok of RD.Set.t

let unreachable = Unreachable

let empty = Ok RD.Set.empty

let create rd_set = Ok rd_set

let canonicalise t =
  (* Users of canonicalised sets aren't interested in what the set contains
     for portions of dead code, so for [Unreachable], we can just return the
     empty set. *)
  match t with
  | Unreachable -> RD.Canonical_set.empty
  | Ok rd_set -> RD.Canonical_set.of_set rd_set

let print ~print_reg:_ ppf = function
  | Unreachable -> Format.fprintf ppf "<unreachable>"
  | Ok rd_set -> RD.Set.print ppf rd_set

let equal t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD.Set.equal regs1 regs2

let map t ~f =
  match t with
  | Ok t -> Ok (f t)
  | Unreachable -> Unreachable

let union regs1 regs2 =
  match regs1, regs2 with
  | Unreachable, _ -> regs1
  | _, Unreachable -> regs2
  | Ok avail1, Ok avail2 -> Ok (RD.Set.union avail1 avail2)

let inter regs1 regs2 =
  match regs1, regs2 with
  | Unreachable, _ -> regs2
  | _, Unreachable -> regs1
  | Ok avail1, Ok avail2 -> Ok (RD.Set.inter avail1 avail2)

let find_reg t reg =
  match t with
  | Unreachable -> None
  | Ok avail -> RD.Set.find_reg avail reg

let made_unavailable_by_clobber t ~regs_clobbered ~register_class =
  match t with
  | Unreachable -> Unreachable
  | Ok rd_set ->
    Ok (RD.Set.made_unavailable_by_clobber rd_set ~regs_clobbered
      ~register_class)

let subset t1 t2 =
  equal (inter t1 t2) t1
