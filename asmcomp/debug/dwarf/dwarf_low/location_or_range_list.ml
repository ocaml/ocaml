(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives

module Make (Entry : Location_or_range_list_entry.S) = struct
  type t = Entry.t list

  let create () = []

  let add t entry = entry :: t

  let section = Entry.section

  let size t =
    List.fold_left (fun size entry ->
        Dwarf_int.add size (Entry.size entry))
      (Dwarf_int.zero ())
      t

  let emit t =
    A.comment "Start of list:";
    A.new_line ();
    List.iter (fun entry ->
        Entry.emit entry)
      (List.rev t)
end
