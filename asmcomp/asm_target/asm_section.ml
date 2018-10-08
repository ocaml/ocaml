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
  | Debug_loc
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

let all_sections_in_order = [
  Text;
  Data;
  Read_only_data;
  Eight_byte_literals;
  Sixteen_byte_literals;
  Jump_tables;
  DWARF Debug_info;
  DWARF Debug_abbrev;
  DWARF Debug_aranges;
  DWARF Debug_loc;
  DWARF Debug_str;
  DWARF Debug_line;
]

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
        | Debug_loc -> "__debug_loc"
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
        | Debug_loc -> ".debug_loc"
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

let text_label () = Asm_label.create ()
let data_label () = Asm_label.create ()
let read_only_data_label () = Asm_label.create ()
let eight_byte_literals_label () = Asm_label.create ()
let sixteen_byte_literals_label () = Asm_label.create ()
let jump_tables_label () = Asm_label.create ()
let debug_info_label () = Asm_label.create ()
let debug_abbrev_label () = Asm_label.create ()
let debug_aranges_label () = Asm_label.create ()
let debug_loc_label () = Asm_label.create ()
let debug_str_label () = Asm_label.create ()
let debug_line_label () = Asm_label.create ()

let label t =
  match t with
  | Text -> text_label ()
  | Data -> data_label ()
  | Read_only_data -> read_only_data_label ()
  | Eight_byte_literals -> eight_byte_literals_label ()
  | Sixteen_byte_literals -> sixteen_byte_literals_label ()
  | Jump_tables -> jump_tables_label ()
  | DWARF Debug_info -> debug_info_label ()
  | DWARF Debug_abbrev -> debug_abbrev_label ()
  | DWARF Debug_aranges -> debug_aranges_label ()
  | DWARF Debug_loc -> debug_loc_label ()
  | DWARF Debug_str -> debug_str_label ()
  | DWARF Debug_line -> debug_line_label ()
