(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  mutable lists : (int * Location_list_entry.t list) list;
  mutable next_offset : int;
}

let create () =
  { lists = []; next_offset = 0 }

let add_location_list t entries =
  let offset = t.next_offset in
  t.lists <- (offset, entries) :: t.lists;
  (* Estimate size: each entry has addresses (8 bytes each) + length (2 bytes) + location bytes *)
  let entry_size entry =
    8 + 8 + 2 + Bytes.length (Location_list_entry.location entry)
  in
  let list_size = List.fold_left (fun acc entry -> acc + entry_size entry) 0 entries in
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
  Format.fprintf ppf "LocationListTable(%d lists)" (count t)
