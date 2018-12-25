(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type label =
  | Int of int
  | String of string

type t = {
  section : Asm_section.t;
  label : label;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Stdlib.compare t1.label t2.label

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash t.label

  let print ppf t =
    match t.label with
    | Int i -> Format.fprintf ppf "L%d" i
    | String s -> Format.fprintf ppf "L%s" s

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let create_int section label =
  { section;
    label = Int label;
  }

let create_string section label =
  { section;
    label = String label;
  }

let label_prefix =
  match Target_system.architecture () with
  | IA32 | X86_64 ->
    begin match Target_system.system () with
    | Linux
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Generic_BSD
    | Solaris
    | BeOS
    | GNU
    | Dragonfly
    | Unknown -> ".L"
    | MacOS_like
    | Windows Native -> "L"
    end
  | ARM
  | AArch64
  | POWER
  | Z -> ".L"

let encode (t : t) =
  match t.label with
  | Int label -> label_prefix ^ (string_of_int label)
  | String label -> label_prefix ^ label

let section t = t.section

let new_label_ref = ref None

let not_initialized () =
  Misc.fatal_error "[Asm_label.initialize] has not been called"

let initialize ~new_label =
  new_label_ref := Some new_label

let create section =
  match !new_label_ref with
  | None -> not_initialized ()
  | Some new_label -> create_int section (new_label ())

let text_label = lazy (create Text)
let data_label = lazy (create Data)
let read_only_data_label = lazy (create Read_only_data)
let eight_byte_literals_label = lazy (create Eight_byte_literals)
let sixteen_byte_literals_label = lazy (create Sixteen_byte_literals)
let jump_tables_label = lazy (create Jump_tables)
let debug_info_label = lazy (create (DWARF Debug_info))
let debug_abbrev_label = lazy (create (DWARF Debug_abbrev))
let debug_aranges_label = lazy (create (DWARF Debug_aranges))
let debug_addr_label = lazy (create (DWARF Debug_addr))
let debug_loc_label = lazy (create (DWARF Debug_loc))
let debug_ranges_label = lazy (create (DWARF Debug_ranges))
let debug_loclists_label = lazy (create (DWARF Debug_loclists))
let debug_rnglists_label = lazy (create (DWARF Debug_rnglists))
let debug_str_label = lazy (create (DWARF Debug_str))
let debug_line_label = lazy (create (DWARF Debug_line))

let for_dwarf_section (dwarf_section : Asm_section.dwarf_section) =
  match dwarf_section with
  | Debug_info -> Lazy.force debug_info_label
  | Debug_abbrev -> Lazy.force debug_abbrev_label
  | Debug_aranges -> Lazy.force debug_aranges_label
  | Debug_addr -> Lazy.force debug_addr_label
  | Debug_loc -> Lazy.force debug_loc_label
  | Debug_ranges -> Lazy.force debug_ranges_label
  | Debug_loclists -> Lazy.force debug_loclists_label
  | Debug_rnglists -> Lazy.force debug_rnglists_label
  | Debug_str -> Lazy.force debug_str_label
  | Debug_line -> Lazy.force debug_line_label

let for_section (section : Asm_section.t) =
  match section with
  | Text -> Lazy.force text_label
  | Data -> Lazy.force data_label
  | Read_only_data -> Lazy.force read_only_data_label
  | Eight_byte_literals -> Lazy.force eight_byte_literals_label
  | Sixteen_byte_literals -> Lazy.force sixteen_byte_literals_label
  | Jump_tables -> Lazy.force jump_tables_label
  | DWARF dwarf_section -> for_dwarf_section dwarf_section
