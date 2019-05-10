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

type t =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables

let all_sections_in_order () =
  [ Text;
    Data;
    Read_only_data;
    Eight_byte_literals;
    Sixteen_byte_literals;
    Jump_tables;
  ]

let section_is_text = function
  | Text
  | Jump_tables -> true
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals -> false

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

let flags t =
  let text () = [".text"], None, [] in
  let data () = [".data"], None, [] in
  let rodata () = [".rodata"], None, [] in
  let system = derived_system () in
  let names, flags, args =
    match t, Target_system.architecture (), system with
    | Text, _, _ -> text ()
    | Data, _, _ -> data ()
    | (Eight_byte_literals | Sixteen_byte_literals), (ARM | AArch64 | Z), _
    | (Eight_byte_literals | Sixteen_byte_literals), _, Solaris ->
      rodata ()
    | Sixteen_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal16"], None, ["16byte_literals"]
    | Sixteen_byte_literals, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Sixteen_byte_literals, _, (MinGW_32 | Win32 | Win64) -> data ()
    | Sixteen_byte_literals, _, _ -> [".rodata.cst8"], Some "a", ["@progbits"]
    | Eight_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal8"], None, ["8byte_literals"]
    | Eight_byte_literals, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Eight_byte_literals, _, (MinGW_32 | Win32 | Win64) -> data ()
    | Eight_byte_literals, _, _ -> [".rodata.cst8"], Some "a", ["@progbits"]
    | Jump_tables, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Jump_tables, _, (MinGW_32 | Win32) -> data ()
    | Jump_tables, _, (MacOS_like | Win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _, _ -> [".rodata"], None, []
    | Read_only_data, _, (MinGW_32 | Win32) -> data ()
    | Read_only_data, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Read_only_data, _, _ -> rodata ()
  in
  { names; flags; args; }

let to_string t =
  let { names; flags = _; args = _; } = flags t in
  String.concat " " names

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    let str =
      match t with
      | Text -> "Text"
      | Data -> "Data"
      | Read_only_data -> "Read_only_data"
      | Eight_byte_literals -> "Eight_byte_literals"
      | Sixteen_byte_literals -> "Sixteen_byte_literals"
      | Jump_tables -> "Jump_tables"
    in
    Format.pp_print_string ppf str

  let output chan t =
    Format.fprintf (Format.formatter_of_out_channel chan) "%a" print t

  let compare t1 t2 = Stdlib.compare t1 t2
  let equal t1 t2 = Stdlib.compare t1 t2 = 0
  let hash t = Hashtbl.hash t
end)
