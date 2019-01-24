(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  dbg: Debuginfo.t;
  phantom_available_before: Backend_var.Set.t;
  available_before: Reg_availability_set.t;
  available_across: Reg_availability_set.t option;
}

let create dbg ~phantom_available_before =
  { dbg;
    phantom_available_before;
    available_before = Reg_availability_set.Unreachable;
    available_across = None;
  }

let none =
  { dbg = Debuginfo.none;
    phantom_available_before = Backend_var.Set.empty;
    available_before = Reg_availability_set.Unreachable;
    available_across = None;
  }

let dbg t = t.dbg
let phantom_available_before t = t.phantom_available_before
let available_before t = t.available_before
let available_across t = t.available_across

let with_available_before t available_before =
  { t with available_before; }

let with_available_across t available_across =
  { t with available_across; }

let map_available_before t ~f =
  with_available_before t (f t.available_before)
