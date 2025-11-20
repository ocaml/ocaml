(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  mutable lists : (int * Range_list_entry.t list) list;
  mutable next_offset : int;
}

let create () =
  { lists = []; next_offset = 0 }

let add_range_list t entries =
  let offset = t.next_offset in
  t.lists <- (offset, entries) :: t.lists;
  (* Each entry has two addresses (8 bytes each) *)
  let entry_size = 16 in
  let list_size = List.length entries * entry_size in
  (* Add terminator (two zero addresses) *)
  let total_size = list_size + 16 in
  t.next_offset <- t.next_offset + total_size;
  offset

let get_all t =
  List.rev t.lists

let is_empty t =
  t.lists = []

let count t =
  List.length t.lists

let print ppf t =
  Format.fprintf ppf "RangeListTable(%d lists)" (count t)
