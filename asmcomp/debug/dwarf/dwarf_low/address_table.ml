(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Asm_directives
module Uint8 = Numbers.Uint8

module Entry = struct
  type t = {
    addr : Asm_label.t;
    adjustment : int;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare
          { addr = addr1; adjustment = adjustment1; }
          { addr = addr2; adjustment = adjustment2; } =
      let c = Asm_label.compare addr1 addr2 in
      if c <> 0 then c
      else Stdlib.compare adjustment1 adjustment2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash { addr; adjustment; } =
      Hashtbl.hash (Asm_label.hash addr, adjustment)

    let print _ _ = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

type entry_and_soc_symbol = {
  entry : Entry.t;
  start_of_code_symbol : Asm_symbol.t;
}

type t = {
  base_addr : Asm_label.t;
  mutable next_index : Address_index.t;
  mutable table : entry_and_soc_symbol Address_index.Map.t;
  mutable rev_table : Address_index.t Entry.Map.t;
}

let create () =
  { base_addr = Asm_label.create (DWARF Debug_addr);
    next_index = Address_index.zero;
    table = Address_index.Map.empty;
    rev_table = Entry.Map.empty;
  }

let add ?(adjustment = 0) t ~start_of_code_symbol addr =
  let entry : Entry.t =
    { addr;
      adjustment;
    }
  in
  match Entry.Map.find entry t.rev_table with
  | exception Not_found ->
    let index = t.next_index in
    t.next_index <- Address_index.succ index;
    t.rev_table <- Entry.Map.add entry index t.rev_table;
    let entry : entry_and_soc_symbol =
      { entry;
        start_of_code_symbol;
      }
    in
    t.table <- Address_index.Map.add index entry t.table;
    index
  | index -> index

let base_addr t = t.base_addr

let initial_length t =
  let num_entries = Int64.of_int (Address_index.Map.cardinal t.table) in
  let size_entries = Int64.mul num_entries (Int64.of_int Arch.size_addr) in
  Initial_length.create (Dwarf_int.of_int64_exn (Int64.add 4L size_entries))

let size t =
  let initial_length = initial_length t in
  Dwarf_int.add (Initial_length.size initial_length)
    (Initial_length.to_dwarf_int initial_length)

let entry_to_dwarf_value (entry : entry_and_soc_symbol) =
  let adjustment = Targetint.of_int_exn entry.entry.adjustment in
  Dwarf_value.code_address_from_label_symbol_diff
    ~comment:"ending address"
    ~upper:entry.entry.addr
    ~lower:entry.start_of_code_symbol
    ~offset_upper:adjustment
    ()

let emit t =
  Initial_length.emit (initial_length t);
  Dwarf_version.emit Dwarf_version.five;
  A.uint8 (Uint8.of_int_exn Arch.size_addr);
  A.uint8 Uint8.zero;
  A.define_label t.base_addr;
  Address_index.Map.iter (fun _index entry ->
      Dwarf_value.emit (entry_to_dwarf_value entry))
    t.table
