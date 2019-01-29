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

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_addr
  | Debug_loc
  | Debug_ranges
  | Debug_loclists
  | Debug_rnglists
  | Debug_str
  | Debug_line

type t =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF of dwarf_section

let all_sections_in_order () =
  let sections = [
    Text;
    Data;
    Read_only_data;
    Eight_byte_literals;
    Sixteen_byte_literals;
    Jump_tables;
    DWARF Debug_info;
    DWARF Debug_abbrev;
    DWARF Debug_aranges;
    DWARF Debug_str;
    DWARF Debug_line;
  ] in
  let dwarf_version_dependent_sections =
    match !Clflags.gdwarf_version with
    | Four ->
      [ DWARF Debug_loc;
        DWARF Debug_ranges;
      ]
    | Five ->
      [ DWARF Debug_addr;
        DWARF Debug_loclists;
        DWARF Debug_rnglists;
      ]
  in
  sections @ dwarf_version_dependent_sections

let section_is_text = function
  | Text -> true
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF _ -> false

(* Modified version of [TS.system] for easier matching in
   [switch_to_section], below. *)
type derived_system =
  | Linux
  | MinGW_32
  | MinGW_64
  | Win32
  | Win64
  | Cygwin
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

let derived_system () =
  match Target_system.system (), Target_system.machine_width () with
  | Linux, _ -> Linux
  | Windows Cygwin, _ -> Cygwin
  | Windows MinGW, Thirty_two -> MinGW_32
  | Windows MinGW, Sixty_four -> MinGW_64
  | Windows Native, Thirty_two -> Win32
  | Windows Native, Sixty_four -> Win64
  | MacOS_like, _ -> MacOS_like
  | FreeBSD, _ -> FreeBSD
  | NetBSD, _ -> NetBSD
  | OpenBSD, _ -> OpenBSD
  | Generic_BSD, _ -> Generic_BSD
  | Solaris, _ -> Solaris
  | Dragonfly, _ -> Dragonfly
  | GNU, _ -> GNU
  | BeOS, _ -> BeOS
  | Unknown, _ -> Unknown

type flags_for_section = {
  names : string list;
  flags : string option;
  args : string list;
}

let flags t ~first_occurrence =
  let text () = [".text"], None, [] in
  let data () = [".data"], None, [] in
  let rodata () = [".rodata"], None, [] in
  let system = derived_system () in
  let names, flags, args =
    match t, Target_system.architecture (), system with
    | Text, _, _ -> text ()
    | Data, _, _ -> data ()
    | DWARF dwarf, _, MacOS_like ->
      let name =
        match dwarf with
        | Debug_info -> "__debug_info"
        | Debug_abbrev -> "__debug_abbrev"
        | Debug_aranges -> "__debug_aranges"
        | Debug_addr -> "__debug_addr"
        | Debug_loc -> "__debug_loc"
        | Debug_ranges -> "__debug_ranges"
        | Debug_loclists -> "__debug_loclists"
        | Debug_rnglists -> "__debug_rnglists"
        | Debug_str -> "__debug_str"
        | Debug_line -> "__debug_line"
      in
      ["__DWARF"; name], None, ["regular"; "debug"]
    | DWARF dwarf, _, _ ->
      let name =
        match dwarf with
        | Debug_info -> ".debug_info"
        | Debug_abbrev -> ".debug_abbrev"
        | Debug_aranges -> ".debug_aranges"
        | Debug_addr -> ".debug_addr"
        | Debug_loc -> ".debug_loc"
        | Debug_ranges -> ".debug_ranges"
        | Debug_loclists -> ".debug_loclists"
        | Debug_rnglists -> ".debug_rnglists"
        | Debug_str -> ".debug_str"
        | Debug_line -> ".debug_line"
      in
      let flags =
        if first_occurrence then
          Some ""
        else
          None
      in
      let args =
        if first_occurrence then
          ["%progbits"]
        else
          []
      in
      [name], flags, args
    | (Eight_byte_literals | Sixteen_byte_literals), (ARM | AArch64 | Z), _
    | (Eight_byte_literals | Sixteen_byte_literals), _, Solaris ->
      rodata ()
    | Sixteen_byte_literals, _, MacOS_like ->
      ["__TEXT";"__literal16"], None, ["16byte_literals"]
    | Sixteen_byte_literals, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Sixteen_byte_literals, _, (MinGW_32 | Win32 | Win64) ->
      data ()
    | Sixteen_byte_literals, _, _ ->
      [".rodata.cst8"], Some "a", ["@progbits"]
    | Eight_byte_literals, _, MacOS_like ->
      ["__TEXT";"__literal8"], None, ["8byte_literals"]
    | Eight_byte_literals, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Eight_byte_literals, _, (MinGW_32 | Win32 | Win64) ->
      data ()
    | Eight_byte_literals, _, _ ->
      [".rodata.cst8"], Some "a", ["@progbits"]
    | Jump_tables, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Jump_tables, _, (MinGW_32 | Win32) ->
      data ()
    | Jump_tables, _, (MacOS_like | Win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _, _ ->
      [".rodata"], None, []
    | Read_only_data, _, (MinGW_32 | Win32) ->
      data ()
    | Read_only_data, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Read_only_data, _, _ ->
      rodata ()
  in
  { names; flags; args; }

let to_string t =
  let { names; flags = _; args = _; } = flags t ~first_occurrence:true in
  String.concat " " names

let print ppf t =
  let str =
    match t with
    | Text -> "Text"
    | Data -> "Data"
    | Read_only_data -> "Read_only_data"
    | Eight_byte_literals -> "Eight_byte_literals"
    | Sixteen_byte_literals -> "Sixteen_byte_literals"
    | Jump_tables -> "Jump_tables"
    | DWARF Debug_info -> "(DWARF Debug_info)"
    | DWARF Debug_abbrev -> "(DWARF Debug_abbrev)"
    | DWARF Debug_aranges -> "(DWARF Debug_aranges)"
    | DWARF Debug_addr -> "(DWARF Debug_addr)"
    | DWARF Debug_loc -> "(DWARF Debug_loc)"
    | DWARF Debug_ranges -> "(DWARF Debug_ranges)"
    | DWARF Debug_loclists -> "(DWARF Debug_loclists)"
    | DWARF Debug_rnglists -> "(DWARF Debug_rnglists)"
    | DWARF Debug_str -> "(DWARF Debug_str)"
    | DWARF Debug_line -> "(DWARF Debug_line)"
  in
  Format.pp_print_string ppf str

let compare t1 t2 =
  Stdlib.compare t1 t2

let equal t1 t2 =
  Stdlib.compare t1 t2 = 0
