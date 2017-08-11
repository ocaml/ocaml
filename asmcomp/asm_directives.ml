(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]
(* CR-someday mshinwell: Eliminate uses of [bprintf] from the assembly
   generation code, then enable this warning. *)
[@@@ocaml.warning "-3"]

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module L = Linkage_name
module LR = Linkage_name.With_reloc
module TS = Target_system

let dwarf_supported = not (TS.windows ())

type constant =
  | Const of Targetint.t
  | This
  | Label of Cmm.label
  | Symbol of L.t
  | Symbol_reloc of LR.t
  | Add of constant * constant
  | Sub of constant * constant
  | Div of constant * int

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type power_section =
  | Function_descriptors
  | Table_of_contents

type ia32_section =
  | Non_lazy_symbol_pointers
  | Jump_table

type section =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF of dwarf_section
  | POWER of power_section
  | IA32 of ia32_section

(* Note: the POWER backend relies on .opd being after .data, so an
   alignment constraint can be enforced. *)
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
  POWER Function_descriptors;
  POWER Table_of_contents;
  IA32 Non_lazy_symbol_pointers;
  IA32 Jump_table;
]

let current_section = ref None

let section_is_text = function
  | Text
  | POWER Function_descriptors -> true
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF _
  | POWER _
  | IA32 _ -> false

let current_section_is_text () =
  match !current_section with
  | None ->
    Misc.fatal_error "Asm_directives.initialize has not been called"
  | Some section -> section_is_text section

let text_label = Cmm.new_label ()
let data_label = Cmm.new_label ()
let read_only_data_label = Cmm.new_label ()
let eight_byte_literals_label = Cmm.new_label ()
let sixteen_byte_literals_label = Cmm.new_label ()
let jump_tables_label = Cmm.new_label ()
let debug_info_label = Cmm.new_label ()
let debug_abbrev_label = Cmm.new_label ()
let debug_aranges_label = Cmm.new_label ()
let debug_loc_label = Cmm.new_label ()
let debug_str_label = Cmm.new_label ()
let debug_line_label = Cmm.new_label ()
let power_function_descriptors_label = Cmm.new_label ()
let power_table_of_contents_label = Cmm.new_label ()
let ia32_non_lazy_symbol_pointers_label = Cmm.new_label ()
let ia32_jump_table_label = Cmm.new_label ()

let label_for_section = function
  | Text -> text_label
  | Data -> data_label
  | Read_only_data -> read_only_data_label
  | Eight_byte_literals -> eight_byte_literals_label
  | Sixteen_byte_literals -> sixteen_byte_literals_label
  | Jump_tables -> jump_tables_label
  | DWARF Debug_info -> debug_info_label
  | DWARF Debug_abbrev -> debug_abbrev_label
  | DWARF Debug_aranges -> debug_aranges_label
  | DWARF Debug_loc -> debug_loc_label
  | DWARF Debug_str -> debug_str_label
  | DWARF Debug_line -> debug_line_label
  | POWER Function_descriptors -> power_function_descriptors_label
  | POWER Table_of_contents -> power_table_of_contents_label
  | IA32 Non_lazy_symbol_pointers -> ia32_non_lazy_symbol_pointers_label
  | IA32 Jump_table -> ia32_jump_table_label

let label_prefix =
  match TS.architecture () with
  | IA32 ->
    begin match TS.system () with
    | Linux _
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Other_BSD
    | Solaris
    | BeOS
    | GNU
    | Unknown -> ".L"
    | MacOS_like
    | Windows Native -> "L"
    end
  | X86_64 ->
    begin match TS.system () with
    | Linux _
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Other_BSD
    | Solaris
    | BeOS
    | GNU -> ".L"
    | MacOS_like
    | Windows Native
    | Unknown -> "L"
    end
  | ARM
  | AArch64
  | POWER
  | Z -> ".L"
  | SPARC -> "L"

let string_of_label label_name = label_prefix ^ (string_of_int label_name)

module Directive = struct
  type constant =
    | Const32 of int32
    | Const of int64
    | This
    | Named_thing of string
    | Add of constant * constant
    | Sub of constant * constant
    | Div of constant * int

  type thing_after_label =
    | Code
    | Machine_width_data

  type comment = string

  type t =
    | Align of { bytes : int; }
    | Bytes of string
    | Comment of comment
    | Global of string
    | Const8 of constant
    | Const16 of constant
    | Const32 of constant * (comment option)
    | Const64 of constant * (comment option)
    | New_label of string * thing_after_label
    | Section of string list * string option * string list
    | Space of { bytes : int; }
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of { reg : int; offset : int; }
    | Cfi_startproc
    | File of { file_num : int option; filename : string; }
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | Private_extern of string
    | Size of string * constant
    | Sleb128 of constant
    | Type of string * string
    | Uleb128 of constant
    | Direct_assignment of string * constant

  let bprintf = Printf.bprintf

  let string_of_string_literal s =
    let buf = Buffer.create (String.length s + 2) in
    let last_was_escape = ref false in
    for i = 0 to String.length s - 1 do
      let c = s.[i] in
      if c >= '0' && c <= '9' then
        if !last_was_escape
        then Printf.bprintf buf "\\%o" (Char.code c)
        else Buffer.add_char buf c
      else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
        Buffer.add_char buf c;
        last_was_escape := false
      end else begin
        Printf.bprintf buf "\\%o" (Char.code c);
        last_was_escape := true
      end
    done;
    Buffer.contents buf

  let buf_bytes_directive buf ~directive s =
    let pos = ref 0 in
    for i = 0 to String.length s - 1 do
      if !pos = 0
      then begin
        if i > 0 then Buffer.add_char buf '\n';
        Buffer.add_char buf '\t';
        Buffer.add_string buf directive;
        Buffer.add_char buf '\t';
      end
      else Buffer.add_char buf ',';
      Printf.bprintf buf "%d" (Char.code s.[i]);
      incr pos;
      if !pos >= 16 then begin pos := 0 end
    done

  let gas_comment_opt = function
    | None -> ""
    | Some comment -> Printf.sprintf "\t/* %s */" comment

  let masm_comment_opt = function
    | None -> ""
    | Some comment -> Printf.sprintf "\t; %s" comment

  let rec cst buf (const : constant) =
    match const with
    | Named_thing _ | Const _ | This as c -> scst buf c
    | Const32 c -> bprintf buf "0x%lx" c
    | Add (c1, c2) -> bprintf buf "%a + %a" scst c1 scst c2
    | Sub (c1, c2) -> bprintf buf "%a - %a" scst c1 scst c2
    | Div (c1, c2) -> bprintf buf "%a / %d" scst c1 c2

  and scst buf (const : constant) =
    match const with
    | This -> Buffer.add_string buf "."
    | Named_thing name -> Buffer.add_string buf name
    | Const n -> bprintf buf "0x%Lx" n
    | Const32 c -> bprintf buf "0x%lx" c
    | Add (c1, c2) -> bprintf buf "(%a + %a)" scst c1 scst c2
    | Sub (c1, c2) -> bprintf buf "(%a - %a)" scst c1 scst c2
    | Div (c1, c2) -> bprintf buf "(%a / %d)" scst c1 c2

  let print_gas buf = function
    | Align { bytes = n; } ->
      (* Some assemblers interpret the integer n as a 2^n alignment and
         others as a number of bytes. *)
      let n =
        match TS.assembler (), TS.architecture () with
        | MacOS, _
        | GAS_like, (ARM | AArch64 | POWER) -> Misc.log2 n
        | _, _ -> n
      in
      bprintf buf "\t.align\t%d" n
    | Const8 n -> bprintf buf "\t.byte\t%a" cst n
    | Const16 n ->
      begin match TS.system () with
      | Solaris -> bprintf buf "\t.value\t%a" cst n
      | _ ->
        (* Apple's documentation says that ".word" is i386-specific, so we use
           ".short" instead.
           Additionally, it appears on ARM that ".word" may be 32 bits wide,
           not 16 bits. *)
        bprintf buf "\t.short\t%a" cst n
      end
    | Const32 (n, comment) ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.long\t%a%s" cst n comment
    | Const64 (n, comment) ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.quad\t%a%s" cst n comment
    | Bytes s ->
      begin match TS.system (), TS.architecture () with
      | Solaris, _
      | _, POWER -> buf_bytes_directive buf ~directive:".byte" s
      | _ -> bprintf buf "\t.ascii\t\"%s\"" (string_of_string_literal s)
      end
    | Comment s -> bprintf buf "\t\t\t\t/* %s */" s
    | Global s -> bprintf buf "\t.globl\t%s" s
    | New_label (s, _typ) -> bprintf buf "%s:" s
    | Section ([".data" ], _, _) -> bprintf buf "\t.data"
    | Section ([".text" ], _, _) -> bprintf buf "\t.text"
    | Section (name, flags, args) ->
      bprintf buf "\t.section %s" (String.concat "," name);
      begin match flags with
      | None -> ()
      | Some flags -> bprintf buf ",%S" flags
      end;
      begin match args with
      | [] -> ()
      | _ -> bprintf buf ",%s" (String.concat "," args)
      end
    | Space { bytes; } ->
      begin match TS.system () with
      | Solaris -> bprintf buf "\t.zero\t%d" bytes
      | _ -> bprintf buf "\t.space\t%d" bytes
      end
    | Cfi_adjust_cfa_offset n -> bprintf buf "\t.cfi_adjust_cfa_offset %d" n
    | Cfi_endproc -> bprintf buf "\t.cfi_endproc"
    | Cfi_offset { reg; offset; } ->
      bprintf buf "\t.cfi_offset %d, %d" reg offset
    | Cfi_startproc -> bprintf buf "\t.cfi_startproc"
    | File { file_num = None; filename; } ->
      bprintf buf "\t.file\t\"%s\"" filename
    | File { file_num = Some file_num; filename; } ->
      bprintf buf "\t.file\t%d\t\"%s\""
        file_num (string_of_string_literal filename)
    | Indirect_symbol s -> bprintf buf "\t.indirect_symbol %s" s
    | Loc { file_num; line; col; } ->
      (* PR#7726: Location.none uses column -1, breaks LLVM assembler *)
      if col >= 0 then bprintf buf "\t.loc\t%d\t%d\t%d" file_num line col
      else bprintf buf "\t.loc\t%d\t%d" file_num line
    | Private_extern s -> bprintf buf "\t.private_extern %s" s
(*
    | Set (arg1, arg2) -> bprintf buf "\t.set %s, %a" arg1 cst arg2
*)
    | Size (s, c) -> bprintf buf "\t.size %s,%a" s cst c
    | Sleb128 c -> bprintf buf "\t.sleb128 %a" cst c
    | Type (s, typ) ->
      (* We use the "STT" forms when they are supported as they are
         unambiguous across platforms
         (cf. https://sourceware.org/binutils/docs/as/Type.html ). *)
      bprintf buf "\t.type %s %s" s typ
    | Uleb128 c -> bprintf buf "\t.uleb128 %a" cst c
    | Direct_assignment (var, const) ->
      begin match TS.assembler () with
      | MacOS -> bprintf buf "%s = %a" var cst const
      | _ ->
        Misc.fatal_error "Cannot emit Direct_assignment except on macOS-like \
          assemblers"
      end

  let rec cst buf (const : constant) =
    match const with
    | Named_thing _ | Const _ | This as c -> scst buf c
    | Const32 c -> bprintf buf "0%lxH" c
    | Add (c1, c2) -> bprintf buf "%a + %a" scst c1 scst c2
    | Sub (c1, c2) -> bprintf buf "%a - %a" scst c1 scst c2
    | Div (c1, c2) -> bprintf buf "%a / %d" scst c1 c2

  and scst buf (const : constant) =
    match const with
    | This -> Buffer.add_string buf "THIS BYTE"
    | Named_thing name -> Buffer.add_string buf name
    | Const n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L ->
        Buffer.add_string buf (Int64.to_string n)
    | Const n -> bprintf buf "0%LxH" n
    | Const32 c -> bprintf buf "0%lxH" c
    | Add (c1, c2) -> bprintf buf "(%a + %a)" scst c1 scst c2
    | Sub (c1, c2) -> bprintf buf "(%a - %a)" scst c1 scst c2
    | Div (c1, c2) -> bprintf buf "(%a / %d)" scst c1 c2

  let print_masm buf = function
    | Align { bytes; } -> bprintf buf "\tALIGN\t%d" bytes
    | Bytes s -> buf_bytes_directive buf ~directive:"BYTE" s
    | Comment s -> bprintf buf " ; %s " s
    | Const8 n -> bprintf buf "\tBYTE\t%a" cst n
    | Const16 n -> bprintf buf "\tWORD\t%a" cst n
    | Const32 (n, comment) ->
      let comment = masm_comment_opt comment in
      bprintf buf "\tDWORD\t%a%s" cst n comment
    | Const64 (n, comment) ->
      let comment = masm_comment_opt comment in
      bprintf buf "\tQUAD\t%a%s" cst n comment
    | Global s -> bprintf buf "\tPUBLIC\t%s" s
    | Section ([".data"], None, []) -> bprintf buf "\t.DATA"
    | Section ([".text"], None, []) -> bprintf buf "\t.CODE"
    | Section _ -> assert false
    | Space { bytes; } -> bprintf buf "\tBYTE\t%d DUP (?)" bytes
    | New_label (label, Code) -> bprintf buf "%s:" label
    | New_label (label, Machine_width_data) ->
      begin match TS.machine_width () with
      | Thirty_two -> bprintf buf "%s LABEL DWORD" label
      | Sixty_four -> bprintf buf "%s LABEL QWORD" label
      end
    | Cfi_adjust_cfa_offset _
    | Cfi_endproc
    | Cfi_offset _
    | Cfi_startproc
    | File _
    | Indirect_symbol _
    | Loc _
    | Private_extern _
    | Size _
    | Sleb128 _
    | Type _
    | Uleb128 _
    | Direct_assignment _ ->
      Misc.fatal_error "Unsupported asm directive for MASM"

  let print b t =
    match TS.assembler () with
    | MASM -> print_masm b t
    | MacOS | GAS_like -> print_gas b t
end

let rec lower_constant (cst : constant) : Directive.constant =
  match cst with
  | Const i ->
    begin match Targetint.repr i with
    | Int32 i -> Const32 i
    | Int64 i -> Const i
    end
  | This -> This
  | Label lbl -> Named_thing (string_of_label lbl)
  | Symbol sym -> Named_thing (L.to_string sym)
  | Symbol_reloc sym -> Named_thing (LR.to_string sym)
  | Add (cst1, cst2) -> Add (lower_constant cst1, lower_constant cst2)
  | Sub (cst1, cst2) -> Sub (lower_constant cst1, lower_constant cst2)
  | Div (cst1, cst2) -> Div (lower_constant cst1, cst2)

let emit_ref = ref None

let emit (d : Directive.t) =
  match !emit_ref with
  | Some emit -> emit d
  | None -> Misc.fatal_error "initialize not called"

let section segment flags args = emit (Section (segment, flags, args))
let align ~bytes = emit (Align { bytes; })

let should_generate_cfi () =
  (* We generate CFI info even if we're not generating any other debugging
     information.  This is in fact necessary on macOS, where it may be
     expected that OCaml stack frames are unwindable (see
     testsuite/tests/unwind/README for more information). *)
  Config.asm_cfi_supported

let cfi_adjust_cfa_offset ~bytes =
  if should_generate_cfi () && bytes <> 0 then begin
    emit (Cfi_adjust_cfa_offset bytes)
    end

let cfi_endproc () =
  if should_generate_cfi () then emit Cfi_endproc

let cfi_offset ~reg ~offset =
  if should_generate_cfi () && offset <> 0 then begin
    emit (Cfi_offset { reg; offset; })
  end

let cfi_startproc () =
  if should_generate_cfi () then emit Cfi_startproc

let comment s = emit (Comment s)
let direct_assignment var cst =
  emit (Direct_assignment (LR.to_string var, lower_constant cst))
let file ~file_num ~file_name =
  emit (File { file_num = Some file_num; filename = file_name; })
let global s = emit (Global (L.to_string s))
let indirect_symbol s = emit (Indirect_symbol (L.to_string s))
let loc ~file_num ~line ~col = emit (Loc { file_num; line; col; })
let private_extern s = emit (Private_extern (L.to_string s))
let size name cst = emit (Size (L.to_string name, (lower_constant cst)))
let sleb128 i = emit (Sleb128 (Const i))
let space ~bytes = emit (Space { bytes; })
let string s = emit (Bytes s)
let type_ name ~type_ = emit (Type (L.to_string name, type_))
let uleb128 i = emit (Uleb128 (Const i))

let const8 cst = emit (Const8 (lower_constant cst))
let const16 cst = emit (Const16 (lower_constant cst))
let const32 cst = emit (Const32 (lower_constant cst, None))
let const64 cst = emit (Const64 (lower_constant cst, None))

let const_machine_width cst =
  match TS.machine_width () with
  | Thirty_two -> const32 cst
  | Sixty_four -> const64 cst

let float32 f =
  let comment = Printf.sprintf "%.12f" f in
  emit (Const32 (Const32 (Int32.bits_of_float f), Some comment))

let float64_core f f_int64 =
  match TS.machine_width () with
  | Sixty_four ->
    let comment = Printf.sprintf "%.12g" f in
    emit (Const64 (Const f_int64, Some comment))
  | Thirty_two ->
    let comment_lo = Printf.sprintf "low part of %.12g" f in
    let comment_hi = Printf.sprintf "high part of %.12g" f in
    let lo : Directive.constant =
      Const (Int64.logand f_int64 0xFFFF_FFFFL)
    in
    let hi : Directive.constant =
      Const (Int64.shift_right_logical f_int64 32)
    in
    if Arch.big_endian then begin
      emit (Const32 (hi, Some comment_hi));
      emit (Const32 (lo, Some comment_lo))
    end else begin
      emit (Const32 (lo, Some comment_lo));
      emit (Const32 (hi, Some comment_hi))
    end

let float64 f = float64_core f (Int64.bits_of_float f)
let float64_from_bits f = float64_core (Int64.float_of_bits f) f

let size ?size_of symbol =
  match TS.system () with
  | GNU | Linux _ | FreeBSD | NetBSD | OpenBSD | Other_BSD ->
    let size_of =
      match size_of with
      | None -> symbol
      | Some size_of -> size_of
    in
    size size_of (Sub (This, Symbol symbol))
  | _ -> ()

let label label_name = const_machine_width (Label label_name)

let define_label label_name =
  let typ : Directive.thing_after_label =
    if current_section_is_text () then Code
    else Machine_width_data
  in
  emit (New_label (string_of_label label_name, typ))

let sections_seen = ref []

(* Modified version of [Target_system.system] for easier matching in
   [switch_to_section], below. *)
type derived_system =
  | Linux
  | Cygwin
  | MinGW_32
  | MinGW_64
  | Win32
  | Win64
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Other_BSD
  | Solaris
  | GNU
  | BeOS
  | Unknown

let derived_system () =
  match TS.system (), TS.machine_width () with
  | Linux _, _ -> Linux
  | Windows Cygwin, _ -> Cygwin
  | Windows MinGW, Thirty_two -> MinGW_32
  | Windows MinGW, Sixty_four -> MinGW_64
  | Windows Native, Thirty_two -> Win32
  | Windows Native, Sixty_four -> Win64
  | MacOS_like, _ -> MacOS_like
  | FreeBSD, _ -> FreeBSD
  | NetBSD, _ -> NetBSD
  | OpenBSD, _ -> OpenBSD
  | Other_BSD, _ -> Other_BSD
  | Solaris, _ -> Solaris
  | GNU, _ -> GNU
  | BeOS, _ -> BeOS
  | Unknown, _ -> Unknown

let switch_to_section (section : section) =
  let first_occurrence =
    if List.mem section !sections_seen then false
    else begin
      sections_seen := section::!sections_seen;
      true
    end
  in
  current_section := Some section;
  let section_name, middle_part, attrs =
    let text () = [".text"], None, [] in
    let data () = [".data"], None, [] in
    let rodata () = [".rodata"], None, [] in
    let system = derived_system () in
    match section, TS.architecture (), system with
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
      let middle_part =
        if first_occurrence then
          Some ""
        else
          None
      in
      let attrs =
        if first_occurrence then
          ["%progbits"]
        else
          []
      in
      [name], middle_part, attrs
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
    | Jump_tables, _, (MinGW_32 | Win32) -> (* XXX Cygwin32? *)
      data ()
    | Jump_tables, _, (MacOS_like | Win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _, _ ->
      [".rodata"], None, []
    | Read_only_data, _, (MinGW_32 | Win32) -> (* XXX Cygwin32? *)
      data ()
    | Read_only_data, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Read_only_data, _, _ ->
      rodata ()
    | POWER Function_descriptors, POWER, _ ->
      [".opd"], Some "aw", []
    | POWER Table_of_contents, POWER, _ ->
      [".toc"], Some "aw", []
    | POWER _, _, _ ->
      Misc.fatal_error "Cannot switch to POWER section on non-POWER \
        architecture"
    | IA32 Non_lazy_symbol_pointers, IA32, MacOS_like ->
      [ "__IMPORT"; "__pointers"], None, ["non_lazy_symbol_pointers" ]
    | IA32 Jump_table, IA32, MacOS_like ->
      [ "__IMPORT"; "__jump_table"], None,
        [ "symbol_stubs"; "self_modifying_code+pure_instructions"; "5" ]
    | IA32 _, _, _ ->
      Misc.fatal_error "Cannot switch to IA32 section on non-IA32 \
        architecture or IA32 non-Darwin system"
  in
  emit (Section (section_name, middle_part, attrs));
  if first_occurrence then begin
    define_label (label_for_section section)
  end

let cached_strings = ref ([] : (string * Cmm.label) list)
let temp_var_counter = ref 0

let reset () =
  cached_strings := [];
  sections_seen := [];
  temp_var_counter := 0

let initialize ~(emit : Directive.t -> unit) =
  emit_ref := Some emit;
  reset ();
  begin match TS.assembler () with
  | MASM | MacOS -> ()
  | GAS_like ->
    (* Forward label references are illegal in gas.  Just put them in for
       all assemblers, they won't harm. *)
    List.iter (fun section ->
        match section with
        | Text
        | Data
        | Read_only_data
        | Eight_byte_literals
        | Sixteen_byte_literals
        | Jump_tables -> switch_to_section section
        | DWARF _ ->
          if !Clflags.debug && dwarf_supported then begin
            switch_to_section section
          end
        | POWER (Function_descriptors | Table_of_contents) ->
          begin match TS.architecture () with
          | POWER -> switch_to_section section
          | IA32 | X86_64 | ARM | AArch64 | SPARC | Z -> ()
          end
        | IA32 (Non_lazy_symbol_pointers | Jump_table) ->
          begin match TS.architecture () with
          | IA32 when TS.macos_like () -> switch_to_section section
          | IA32 | POWER | X86_64 | ARM | AArch64 | SPARC | Z -> ()
          end)
      all_sections_in_order
  end;
  emit (File { file_num = None; filename = ""; });  (* PR#7037 *)
  switch_to_section Text

let define_data_symbol symbol =
  emit (New_label (L.to_string symbol, Machine_width_data));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_OBJECT"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let define_function_symbol symbol =
  if not (current_section_is_text ()) then begin
    Misc.fatal_error "define_function_symbol can only be called when \
      emitting to a text section"
  end;
  emit (New_label (L.to_string symbol, Code));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_FUNC"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let symbol sym = const_machine_width (Symbol_reloc sym)

let symbol_plus_offset sym ~offset_in_bytes =
  const_machine_width (Add (Symbol_reloc sym, Const offset_in_bytes))

let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "Ltemp%d" id

(* To avoid callers of this module having to worry about whether operands
   involved in displacement calculations are or are not relocatable, and to
   guard against clever linkers doing e.g. branch relaxation at link time, we
   always force such calculations to be done in a relocatable manner at
   link time.  On macOS this requires use of the "direct assignment"
   syntax rather than ".set": the latter forces expressions to be evaluated
   as absolute assembly-time constants. *)

(* CR mshinwell: This all needs testing for MASM. *)

let force_relocatable expr =
  match TS.assembler () with
  | MacOS ->
    let temp = Linkage_name.create (new_temp_var ()) in
    direct_assignment (LR.no_reloc temp) expr;
    Symbol_reloc
      (LR.no_reloc temp)  (* not really a symbol, but OK (same below) *)
  | GAS_like | MASM ->
    expr

let between_symbols ~upper ~lower =
  let expr = Sub (Symbol_reloc upper, Symbol_reloc lower) in
  const_machine_width (force_relocatable expr)

let between_labels_32bit ~upper ~lower =
  let expr = Sub (Label upper, Label lower) in
  const32 (force_relocatable expr)

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let expr =
    Sub (
      Add (Label upper, Const offset_upper),
      Symbol_reloc lower)
  in
  const_machine_width (force_relocatable expr)

let between_symbol_and_label_offset' ~upper ~lower ~offset_lower =
  let expr =
    Sub (
      Symbol_reloc upper,
      Add (Label lower, Const offset_lower))
  in
  const_machine_width (force_relocatable expr)

let between_this_and_label_offset_32bit ~upper ~offset_upper =
  let expr =
    Sub (Add (Label upper, Const offset_upper), This)
  in
  const32 (force_relocatable expr)

let scaled_distance_between_this_and_label_offset ~upper ~divide_by =
  let expr = Div (Sub (Label upper, This), divide_by) in
  const_machine_width (force_relocatable expr)

let constant_with_width expr ~(width : Target_system.machine_width) =
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width. *)
  | Thirty_two -> const32 expr
  | Sixty_four -> const64 expr

let offset_into_section_label ~section ~label:upper ~width =
  let lower = label_for_section section in
  let expr : constant =
    (* The meaning of a label reference depends on the assembler:
       - On Mac OS X, it appears to be the distance from the label back to
         the start of the assembly file.
       - On gas, it is the distance from the label back to the start of the
         current section. *)
    match TS.assembler () with
    | MacOS ->
      let temp = Linkage_name.create (new_temp_var ()) in
      direct_assignment (LR.no_reloc temp) (Sub (Label upper, Label lower));
      Symbol_reloc (LR.no_reloc temp)
    | GAS_like | MASM ->
      Label upper
  in
  constant_with_width expr ~width

let offset_into_section_symbol ~section ~symbol:upper ~width =
  let lower = label_for_section section in
  let expr : constant =
    (* The same thing as for [offset_into_section_label] applies here. *)
    match TS.assembler () with
    | MacOS ->
      let temp = Linkage_name.create (new_temp_var ()) in
      direct_assignment (LR.no_reloc temp)
        (Sub (Symbol_reloc upper, Label lower));
      Symbol_reloc (LR.no_reloc temp)
    | GAS_like | MASM -> Symbol_reloc upper
  in
  constant_with_width expr ~width

let int8 i =
  const8 (Const (Targetint.of_int (Int8.to_int i)))

let int16 i =
  const16 (Const (Targetint.of_int (Int16.to_int i)))

let int32 i =
  const32 (Const (Targetint.of_int32 i))

let int64 i =
  const64 (Const (Targetint.of_int64 i)) (* XXX Incorrect *)

let targetint n =
  match TS.machine_width () with
  | Thirty_two -> const32 (Const n)
  | Sixty_four -> const64 (Const n)

let cache_string str =
  match List.assoc str !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Cmm.new_label () in
    cached_strings := (str, label) :: !cached_strings;
    label

let emit_cached_strings () =
  List.iter (fun (str, label_name) ->
      define_label label_name;
      string str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := []

let mark_stack_non_executable () =
  let current_section = !current_section in
  match TS.system () with
  | Linux _ ->
    section [".note.GNU-stack"] (Some "") [ "%progbits" ];
    begin match current_section with
    | None -> ()
    | Some current_section -> switch_to_section current_section
    end
  | _ -> ()
