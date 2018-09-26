(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]
(* CR-someday mshinwell: Eliminate uses of [bprintf] from the assembly
   generation code, then enable this warning. *)
[@@@ocaml.warning "-3"]

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module TS = Target_system

let dwarf_supported () =
  match TS.object_file_format_and_abi () with
  | ELF _ | Mach_O -> true
  | A_out | PE | Unknown -> false

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
  | IA32 | X86_64 ->
    begin match TS.system () with
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

let string_of_label label_name = label_prefix ^ (string_of_int label_name)

let bprintf = Printf.bprintf

module Directive = struct
  module Constant = struct
    type t =
      | Signed_int of Int64.t
      | This
      | Named_thing of string
      | Add of t * t
      | Sub of t * t
      | Div of t * int

    let rec print buf t =
      match t with
      | Named_thing _ | Signed_int _ | This as c -> print_subterm buf c
      | Add (c1, c2) ->
        bprintf buf "%a + %a" print_subterm c1 print_subterm c2
      | Sub (c1, c2) ->
        bprintf buf "%a - %a" print_subterm c1 print_subterm c2
      | Div (c1, c2) ->
        bprintf buf "%a / %d" print_subterm c1 c2

    and print_subterm buf t =
      match t with
      | This ->
        begin match TS.assembler () with
        | MacOS | GAS_like -> Buffer.add_string buf "."
        | MASM -> Buffer.add_string buf "THIS BYTE"
        end
      | Named_thing name -> Buffer.add_string buf name
      | Signed_int n ->
        begin match TS.assembler () with
        | MacOS | GAS_like -> bprintf buf "0x%Lx" n
        | MASM ->
          if n >= -0x8000_0000L && n <= 0x7fff_ffffL then
            Buffer.add_string buf (Int64.to_string n)
          else
            bprintf buf "0%LxH" n
        end
      | Add (c1, c2) ->
        bprintf buf "(%a + %a)" print_subterm c1 print_subterm c2
      | Sub (c1, c2) ->
        bprintf buf "(%a - %a)" print_subterm c1 print_subterm c2
      | Div (c1, c2) ->
        bprintf buf "(%a / %d)" print_subterm c1 c2

    let rec evaluate t =
      let (>>=) = Misc.Stdlib.Option.(>>=) in
      match t with
      | Signed_int i -> Some i
      | This -> None
      | Named_thing _ -> None
      | Add (t1, t2) ->
        evaluate t1
        >>= fun i1 ->
        evaluate t2
        >>= fun i2 ->
        Some (Int64.add i1 i2)
      | Sub (t1, t2) ->
        evaluate t1
        >>= fun i1 ->
        evaluate t2
        >>= fun i2 ->
        Some (Int64.sub i1 i2)
      | Div (t, divisor) ->
        evaluate t
        >>= fun i ->
        if divisor = 0 then begin
          Misc.fatal_error "Division by zero when evaluating constant"
        end;
        Some (Int64.div i (Int64.of_int divisor))
  end

  module Constant_with_width = struct
    type width_in_bytes =
      | Eight
      | Sixteen
      | Thirty_two
      | Sixty_four

    let int_of_width_in_bytes = function
      | Eight -> 8
      | Sixteen -> 16
      | Thirty_two -> 32
      | Sixty_four -> 64

    type t = {
      constant : Constant.t;
      width_in_bytes : width_in_bytes;
    }

    let create constant width_in_bytes =
      begin match Constant.evaluate constant with
      | None -> ()
      | Some n ->
        let in_range =
          match width_in_bytes with
          | Eight -> n >= -0x80L && n <= 0x7fL
          | Sixteen -> n >= -0x8000L && n <= 0x7fffL
          | Thirty_two -> n >= -0x8000_0000L && n <= 0x7fff_ffffL
          | Sixty_four -> true
        in
        if not in_range then begin
          Misc.fatal_errorf "Signed integer constant %Ld does not fit in \
              %d bits"
            n (int_of_width_in_bytes width_in_bytes)
        end
      end;
      { constant;
        width_in_bytes;
      }

    let constant t = t.constant
    let width_in_bytes t = t.width_in_bytes
  end

  type thing_after_label =
    | Code
    | Machine_width_data

  type comment = string

  type t =
    | Align of { bytes : int; }
    | Bytes of string
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of { reg : int; offset : int; }
    | Cfi_startproc
    | Comment of comment
    | Const of { constant : Constant_with_width.t; comment : string option; }
    | Direct_assignment of string * Constant.t
    | File of { file_num : int option; filename : string; }
    | Global of string
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | New_label of string * thing_after_label
    | Private_extern of string
    | Section of {
        names : string list;
        flags : string option;
        args : string list;
      }
    | Size of string * Constant.t
    | Sleb128 of Constant.t
    | Space of { bytes : int; }
    | Type of string * string
    | Uleb128 of Constant.t

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

  let print_gas buf t =
    let gas_comment_opt = function
      | None -> ""
      | Some comment -> Printf.sprintf "\t/* %s */" comment
    in
    match t with
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
    | Const { constant; comment; } ->
      let directive =
        match Constant_with_width.width_in_bytes constant with
        | Eight -> "byte"
        | Sixteen ->
          begin match TS.system () with
          | Solaris -> "value"
          | _ ->
            (* Apple's documentation says that ".word" is i386-specific, so
               we use ".short" instead.
               Additionally, it appears on ARM that ".word" may be 32 bits wide,
               not 16 bits. *)
            "short"
          end
        | Thirty_two -> "long"
        | Sixty_four -> "quad"
      in
      let comment = gas_comment_opt comment in
      bprintf buf "\t.%s\t%a%s"
        directive
        Constant.print (Constant_with_width.constant constant)
        comment
    | Bytes s ->
      begin match TS.system (), TS.architecture () with
      | Solaris, _
      | _, POWER -> buf_bytes_directive buf ~directive:".byte" s
      | _ -> bprintf buf "\t.ascii\t\"%s\"" (string_of_string_literal s)
      end
    | Comment s -> bprintf buf "\t\t\t\t/* %s */" s
    | Global s -> bprintf buf "\t.globl\t%s" s
    | New_label (s, _typ) -> bprintf buf "%s:" s
    | Section { names = [".data"]; _ } -> bprintf buf "\t.data"
    | Section { names = [".text"]; _ } -> bprintf buf "\t.text"
    | Section { names; flags; args; } ->
      bprintf buf "\t.section %s" (String.concat "," names);
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
    | Size (s, c) -> bprintf buf "\t.size %s,%a" s Constant.print c
    | Sleb128 c -> bprintf buf "\t.sleb128 %a" Constant.print c
    | Type (s, typ) ->
      (* We use the "STT" forms when they are supported as they are
         unambiguous across platforms
         (cf. https://sourceware.org/binutils/docs/as/Type.html ). *)
      bprintf buf "\t.type %s %s" s typ
    | Uleb128 c -> bprintf buf "\t.uleb128 %a" Constant.print c
    | Direct_assignment (var, const) ->
      begin match TS.assembler () with
      | MacOS -> bprintf buf "%s = %a" var Constant.print const
      | _ ->
        Misc.fatal_error "Cannot emit [Direct_assignment] except on macOS-like \
          assemblers"
      end

  let print_masm buf t =
    let masm_comment_opt = function
      | None -> ""
      | Some comment -> Printf.sprintf "\t; %s" comment
    in
    match t with
    | Align { bytes; } -> bprintf buf "\tALIGN\t%d" bytes
    | Bytes s -> buf_bytes_directive buf ~directive:"BYTE" s
    | Comment s -> bprintf buf " ; %s " s
    | Const { constant; comment; } ->
      let directive =
        match Constant_with_width.width_in_bytes constant with
        | Eight -> "BYTE"
        | Sixteen -> "WORD"
        | Thirty_two -> "DWORD"
        | Sixty_four -> "QWORD"
      in
      let comment = masm_comment_opt comment in
      bprintf buf "\t%s\t%a%s"
        directive
        Constant.print (Constant_with_width.constant constant)
        comment
    | Global s -> bprintf buf "\tPUBLIC\t%s" s
    | Section { names = [".data"]; _ } -> bprintf buf "\t.DATA"
    | Section { names = [".text"]; _ } -> bprintf buf "\t.CODE"
    | Section _ -> assert false
    | Space { bytes; } -> bprintf buf "\tBYTE\t%d DUP (?)" bytes
    | New_label (label, Code) -> bprintf buf "%s:" label
    | New_label (label, Machine_width_data) ->
      begin match TS.machine_width () with
      | Thirty_two -> bprintf buf "%s LABEL DWORD" label
      | Sixty_four -> bprintf buf "%s LABEL QWORD" label
      end
    | Cfi_adjust_cfa_offset _ ->
      Misc.fatal_error "Unsupported asm directive [Cfi_adjust_cfa_offset] \
        for MASM"
    | Cfi_endproc ->
      Misc.fatal_error "Unsupported asm directive [Cfi_endproc] for MASM"
    | Cfi_offset _ ->
      Misc.fatal_error "Unsupported asm directive [Cfi_offset] for MASM"
    | Cfi_startproc ->
      Misc.fatal_error "Unsupported asm directive [Cfi_startproc] for MASM"
    | File _ ->
      Misc.fatal_error "Unsupported asm directive [File] for MASM"
    | Indirect_symbol _ ->
      Misc.fatal_error "Unsupported asm directive [Indirect_symbol] for MASM"
    | Loc _ ->
      Misc.fatal_error "Unsupported asm directive [Loc] for MASM"
    | Private_extern _ ->
      Misc.fatal_error "Unsupported asm directive [Private_extern] for MASM"
    | Size _ ->
      Misc.fatal_error "Unsupported asm directive [Size] for MASM"
    | Sleb128 _ ->
      Misc.fatal_error "Unsupported asm directive [Sleb128] for MASM"
    | Type _ ->
      Misc.fatal_error "Unsupported asm directive [Type] for MASM"
    | Uleb128 _ ->
      Misc.fatal_error "Unsupported asm directive [Uleb128] for MASM"
    | Direct_assignment _ ->
      Misc.fatal_error "Unsupported asm directive [Direct_assignment] for MASM"

  let print b t =
    match TS.assembler () with
    | MASM -> print_masm b t
    | MacOS | GAS_like -> print_gas b t
end

(* A higher-level version of [Constant.t] which contains some more
   abstractions (e.g. use of [Cmm.label], and in the future distinguished
   types to represent symbols and symbol references before they are mangled
   to plain [string]s for [Constant.t]). *)
type proto_constant =
  | Signed_int of Int64.t
  | This
  | Label of Cmm.label
  | Symbol of string
  | Symbol_reloc of string
    (* [Symbol_reloc] represents a reference to a symbol with an optional
       relocation suffix.  It is kept separate from [Symbol] to facilitate the
       future introduction of a distinguished type for such references. *)
  | Add of proto_constant * proto_constant
  | Sub of proto_constant * proto_constant
  | Div of proto_constant * int

let rec lower_proto_constant (cst : proto_constant) : Directive.Constant.t =
  match cst with
  | Signed_int n -> Signed_int n
  | This -> This
  | Label lbl -> Named_thing (string_of_label lbl)
  | Symbol sym -> Named_thing sym
  | Symbol_reloc sym -> Named_thing sym
  | Add (cst1, cst2) ->
    Add (lower_proto_constant cst1, lower_proto_constant cst2)
  | Sub (cst1, cst2) ->
    Sub (lower_proto_constant cst1, lower_proto_constant cst2)
  | Div (cst1, cst2) -> Div (lower_proto_constant cst1, cst2)

let emit_ref = ref None

let emit (d : Directive.t) =
  match !emit_ref with
  | Some emit -> emit d
  | None -> Misc.fatal_error "initialize not called"

let emit_non_masm (d : Directive.t) =
  match TS.assembler () with
  | MASM ->  ()
  | MacOS | GAS_like -> emit d

let section ~names ~flags ~args = emit (Section { names; flags; args; })

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

let comment text = emit (Comment text)
let loc ~file_num ~line ~col = emit_non_masm (Loc { file_num; line; col; })
let space ~bytes = emit (Space { bytes; })
let string str = emit (Bytes str)

let global ~symbol = emit (Global symbol)
let indirect_symbol ~symbol = emit (Indirect_symbol symbol)
let private_extern ~symbol = emit (Private_extern symbol)
let size name cst = emit (Size (name, (lower_proto_constant cst)))
let type_ name ~type_ = emit (Type (name, type_))

let sleb128 i = emit (Sleb128 (Directive.Constant.Signed_int i))
let uleb128 i = emit (Uleb128 (Directive.Constant.Signed_int i))

let direct_assignment var cst =
  emit (Direct_assignment (var, lower_proto_constant cst))

let const ?comment constant
      (width : Directive.Constant_with_width.width_in_bytes) =
  let constant = lower_proto_constant constant in
  let constant = Directive.Constant_with_width.create constant width in
  emit (Const { constant; comment; })

let const_machine_width ?comment constant =
  match Target_system.machine_width () with
  | Thirty_two -> const ?comment constant Thirty_two
  | Sixty_four -> const ?comment constant Sixty_four

let float32 f =
  let comment = Printf.sprintf "%.12f" f in
  let f_int32 = Int64.of_int32 (Int32.bits_of_float f) in
  const ~comment (Signed_int f_int32) Sixty_four

let float64_core f f_int64 =
  match TS.machine_width () with
  | Sixty_four ->
    let comment = Printf.sprintf "%.12g" f in
    const ~comment (Signed_int f_int64) Sixty_four
  | Thirty_two ->
    let comment_lo = Printf.sprintf "low part of %.12g" f in
    let comment_hi = Printf.sprintf "high part of %.12g" f in
    let lo = Signed_int (Int64.logand f_int64 0xffff_ffffL) in
    let hi = Signed_int (Int64.shift_right_logical f_int64 32) in
    if Arch.big_endian then begin
      const ~comment:comment_hi hi Thirty_two;
      const ~comment:comment_lo lo Thirty_two
    end else begin
      const ~comment:comment_lo lo Thirty_two;
      const ~comment:comment_hi hi Thirty_two
    end

let float64 f = float64_core f (Int64.bits_of_float f)
let float64_from_bits f = float64_core (Int64.float_of_bits f) f

let size ?size_of_symbol ~symbol =
  match TS.system () with
  | GNU | Linux | FreeBSD | NetBSD | OpenBSD | Generic_BSD ->
    let size_of_symbol =
      match size_of_symbol with
      | None -> symbol
      | Some size_of_symbol -> size_of_symbol
    in
    size size_of_symbol (Sub (This, Symbol symbol))
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
  match TS.system (), TS.machine_width () with
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

let switch_to_section (section : section) =
  let first_occurrence =
    if List.mem section !sections_seen then false
    else begin
      sections_seen := section::!sections_seen;
      true
    end
  in
  current_section := Some section;
  let names, flags, args =
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
  emit (Section { names; flags; args; });
  if first_occurrence then begin
    define_label (label_for_section section)
  end

let cached_strings = ref ([] : (string * Cmm.label) list)
let temp_var_counter = ref 0

let reset () =
  cached_strings := [];
  sections_seen := [];
  temp_var_counter := 0

let file ?file_num ~file_name () =
  emit_non_masm (File { file_num = file_num; filename = file_name; })

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
          if !Clflags.debug && dwarf_supported () then begin
            switch_to_section section
          end
        | POWER (Function_descriptors | Table_of_contents) ->
          begin match TS.architecture () with
          | POWER -> switch_to_section section
          | IA32 | X86_64 | ARM | AArch64 |  Z -> ()
          end
        | IA32 (Non_lazy_symbol_pointers | Jump_table) ->
          begin match TS.architecture () with
          | IA32 when TS.macos_like () -> switch_to_section section
          | IA32 | POWER | X86_64 | ARM | AArch64 | Z -> ()
          end)
      all_sections_in_order
  end;
  file ~file_name:"" ();  (* PR#7037 *)
  switch_to_section Text

let file ~file_num ~file_name = file ~file_num ~file_name ()

let define_data_symbol ~symbol =
  emit (New_label (symbol, Machine_width_data));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_OBJECT"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let define_function_symbol ~symbol =
  if not (current_section_is_text ()) then begin
    Misc.fatal_error "[define_function_symbol] can only be called when \
      emitting to a text section"
  end;
  emit (New_label (symbol, Code));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_FUNC"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let symbol sym = const_machine_width (Symbol_reloc sym)

let symbol_plus_offset ~symbol ~offset_in_bytes =
  let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
  const_machine_width (Add (Symbol_reloc symbol, Signed_int offset_in_bytes))

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

let force_relocatable expr =
  match TS.assembler () with
  | MacOS ->
    let temp = new_temp_var () in
    direct_assignment temp expr;
    Symbol_reloc temp  (* not really a symbol, but OK (same below) *)
  | GAS_like | MASM ->
    expr

let between_symbols ~upper ~lower =
  let expr = Sub (Symbol_reloc upper, Symbol_reloc lower) in
  const_machine_width (force_relocatable expr)

let between_labels_32bit ~upper ~lower =
  let expr = Sub (Label upper, Label lower) in
  const (force_relocatable expr) Thirty_two

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let offset_upper = Targetint.to_int64 offset_upper in
  let expr =
    Sub (Add (Label upper, Signed_int offset_upper), Symbol_reloc lower)
  in
  const_machine_width (force_relocatable expr)

let between_symbol_and_label_offset' ~upper ~lower ~offset_lower =
  let offset_lower = Targetint.to_int64 offset_lower in
  let expr =
    Sub (Symbol_reloc upper, Add (Label lower, Signed_int offset_lower))
  in
  const_machine_width (force_relocatable expr)

let between_this_and_label_offset_32bit ~upper ~offset_upper =
  let offset_upper = Targetint.to_int64 offset_upper in
  let expr =
    Sub (Add (Label upper, Signed_int offset_upper), This)
  in
  const (force_relocatable expr) Thirty_two

let scaled_distance_between_this_and_label_offset ~upper ~divide_by =
  let expr = Div (Sub (Label upper, This), divide_by) in
  const_machine_width (force_relocatable expr)

let offset_into_section_label ~section ~label:upper
      ~(width : Target_system.machine_width) =
  let lower = label_for_section section in
  let expr : proto_constant =
    (* The meaning of a label reference depends on the assembler:
       - On Mac OS X, it appears to be the distance from the label back to
         the start of the assembly file.
       - On gas, it is the distance from the label back to the start of the
         current section. *)
    match TS.assembler () with
    | MacOS ->
      let temp = new_temp_var () in
      direct_assignment temp (Sub (Label upper, Label lower));
      Symbol_reloc temp
    | GAS_like | MASM ->
      Label upper
  in
  let width : Directive.Constant_with_width.width_in_bytes =
    match width with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  const expr width

let offset_into_section_symbol ~section ~symbol:upper
      ~(width : Target_system.machine_width) =
  let lower = label_for_section section in
  let expr : proto_constant =
    (* The same thing as for [offset_into_section_label] applies here. *)
    match TS.assembler () with
    | MacOS ->
      let temp = new_temp_var () in
      direct_assignment temp (Sub (Symbol_reloc upper, Label lower));
      Symbol_reloc temp
    | GAS_like | MASM -> Symbol_reloc upper
  in
  let width : Directive.Constant_with_width.width_in_bytes =
    match width with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  const expr width

let int8 i = const (Signed_int (Int64.of_int (Int8.to_int i))) Eight
let int16 i = const (Signed_int (Int64.of_int (Int16.to_int i))) Sixteen
let int32 i = const (Signed_int (Int64.of_int32 i)) Thirty_two
let int64 i = const (Signed_int i) Sixty_four

let targetint n =
  match Targetint.repr n with
  | Int32 n -> int32 n
  | Int64 n -> int64 n

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
  | Linux ->
    section ~names:[".note.GNU-stack"] ~flags:(Some "") ~args:["%progbits"];
    begin match current_section with
    | None -> ()
    | Some current_section -> switch_to_section current_section
    end
  | _ -> ()
