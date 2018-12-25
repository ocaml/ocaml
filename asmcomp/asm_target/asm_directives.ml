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

module Uint64 = Numbers.Uint64

module TS = Target_system

let dwarf_supported () =
  match TS.object_file_format_and_abi () with
  | ELF _ | Mach_O -> true
  | A_out | PE | Unknown -> false

let big_endian_ref = ref None
let current_section_ref = ref None

let not_initialized () =
  Misc.fatal_error "[Asm_directives.initialize] has not been called"

let current_section () =
  match !current_section_ref with
  | None -> not_initialized ()
  | Some section -> section

let current_section_is_text () =
  match !current_section_ref with
  | None -> not_initialized ()
  | Some section -> Asm_section.section_is_text section

let big_endian () =
  match !big_endian_ref with
  | None -> not_initialized ()
  | Some big_endian -> big_endian

let bprintf = Printf.bprintf

module Directive = struct
  module Constant = struct
    type t =
      | Signed_int of Int64.t
      | Unsigned_int of Uint64.t
      | This
      | Named_thing of string
      | Add of t * t
      | Sub of t * t

    let rec print buf t =
      match t with
      | Named_thing _ | Signed_int _ | Unsigned_int _
      | This as c -> print_subterm buf c
      | Add (c1, c2) ->
        bprintf buf "%a + %a" print_subterm c1 print_subterm c2
      | Sub (c1, c2) ->
        bprintf buf "%a - %a" print_subterm c1 print_subterm c2

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
      | Unsigned_int n ->
        (* We can use the printer for [Signed_int] since we always print
           as an unsigned hex representation. *)
        print_subterm buf (Signed_int (Uint64.to_int64 n))
      | Add (c1, c2) ->
        bprintf buf "(%a + %a)" print_subterm c1 print_subterm c2
      | Sub (c1, c2) ->
        bprintf buf "(%a - %a)" print_subterm c1 print_subterm c2

    let rec evaluate t =
      let (>>=) = Misc.Stdlib.Option.(>>=) in
      match t with
      | Signed_int i -> Some i
      | Unsigned_int _ ->
        (* For the moment we don't evaluate arithmetic on unsigned ints. *)
        None
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
    | Bytes of { str : string; comment : string option; }
    | Cfi_adjust_cfa_offset of int
    | Cfi_def_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of { reg : int; offset : int; }
    | Cfi_startproc
    | Comment of comment
    | Const of { constant : Constant_with_width.t; comment : string option; }
    | File of { file_num : int option; filename : string; }
    | Global of string
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | New_label of string * thing_after_label
    | New_line
    | Private_extern of string
    | Section of {
        names : string list;
        flags : string option;
        args : string list;
      }
    | Set of string * Constant.t
    | Size of string * Constant.t
    | Sleb128 of { constant : Constant.t; comment : string option; }
    | Space of { bytes : int; }
    | Type of string * string
    | Uleb128 of { constant : Constant.t; comment : string option; }

  let bprintf = Printf.bprintf

  let emit_comments () = !Clflags.keep_asm_file

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
    let gas_comment_opt comment_opt =
      if not (emit_comments ()) then ""
      else
        match comment_opt with
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
    | Bytes { str; comment; } ->
      begin match TS.system (), TS.architecture () with
      | Solaris, _
      | _, POWER -> buf_bytes_directive buf ~directive:".byte" str
      | _ -> bprintf buf "\t.ascii\t\"%s\"" (string_of_string_literal str)
      end;
      bprintf buf "%s" (gas_comment_opt comment)
    | Comment s -> bprintf buf "\t\t\t/* %s */" s
    | Global s -> bprintf buf "\t.globl\t%s" s
    | New_label (s, _typ) -> bprintf buf "%s:" s
    | New_line -> ()
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
    | Cfi_def_cfa_offset n -> bprintf buf "\t.cfi_def_cfa_offset %d" n
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
    | Sleb128 { constant; comment; } ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.sleb128 %a%s" Constant.print constant comment
    | Type (s, typ) ->
      (* We use the "STT" forms when they are supported as they are
         unambiguous across platforms
         (cf. https://sourceware.org/binutils/docs/as/Type.html ). *)
      bprintf buf "\t.type %s %s" s typ
    | Uleb128 { constant; comment; } ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.uleb128 %a%s" Constant.print constant comment
    | Set (var, const) ->
      begin match TS.assembler () with
      | MacOS -> bprintf buf "%s = %a" var Constant.print const
      | _ ->
        Misc.fatal_error "Cannot emit [Set] except on macOS-like \
          assemblers"
      end

  let print_masm buf t =
    let unsupported name =
      Misc.fatal_errorf "Unsupported asm directive [%s] for MASM" name
    in
    let masm_comment_opt comment_opt =
      if not (emit_comments ()) then ""
      else
        match comment_opt with
        | None -> ""
        | Some comment -> Printf.sprintf "\t; %s" comment
    in
    match t with
    | Align { bytes; } -> bprintf buf "\tALIGN\t%d" bytes
    | Bytes { str; comment; } ->
      buf_bytes_directive buf ~directive:"BYTE" str;
      bprintf buf "%s" (masm_comment_opt comment)
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
    | Section _ -> Misc.fatal_error "Unknown section name for MASM emitter"
    | Space { bytes; } -> bprintf buf "\tBYTE\t%d DUP (?)" bytes
    | New_label (label, Code) -> bprintf buf "%s:" label
    | New_label (label, Machine_width_data) ->
      begin match TS.machine_width () with
      | Thirty_two -> bprintf buf "%s LABEL DWORD" label
      | Sixty_four -> bprintf buf "%s LABEL QWORD" label
      end
    | New_line -> ()
    | Cfi_adjust_cfa_offset _ -> unsupported "Cfi_adjust_cfa_offset"
    | Cfi_def_cfa_offset _ -> unsupported "Cfi_def_cfa_offset"
    | Cfi_endproc -> unsupported "Cfi_endproc"
    | Cfi_offset _ -> unsupported "Cfi_offset"
    | Cfi_startproc -> unsupported "Cfi_startproc"
    | File _ -> unsupported "File"
    | Indirect_symbol _ -> unsupported "Indirect_symbol"
    | Loc _ -> unsupported "Loc"
    | Private_extern _ -> unsupported "Private_extern"
    | Size _ -> unsupported "Size"
    | Sleb128 _ -> unsupported "Sleb128"
    | Type _ -> unsupported "Type"
    | Uleb128 _ -> unsupported "Uleb128"
    | Set _ -> unsupported "Set"

  let print b t =
    match TS.assembler () with
    | MASM -> print_masm b t
    | MacOS | GAS_like -> print_gas b t
end

(* A higher-level version of [Constant.t] which contains some more
   abstractions (e.g. use of [Cmm.label], and in the future distinguished
   types to represent symbols and symbol references before they are mangled
   to plain [string]s for [Constant.t]). *)
type expr =
  | Signed_int of Int64.t
  | Unsigned_int of Uint64.t
  | This
  | Label of Asm_label.t
  | Symbol of Asm_symbol.t
  | Add of expr * expr
  | Sub of expr * expr

let rec lower_expr (cst : expr) : Directive.Constant.t =
  match cst with
  | Signed_int n -> Signed_int n
  | Unsigned_int n -> Unsigned_int n
  | This -> This
  | Label lbl -> Named_thing (Asm_label.encode lbl)
  | Symbol sym -> Named_thing (Asm_symbol.encode sym)
  | Add (cst1, cst2) ->
    Add (lower_expr cst1, lower_expr cst2)
  | Sub (cst1, cst2) ->
    Sub (lower_expr cst1, lower_expr cst2)

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

let cfi_def_cfa_offset ~bytes =
  if should_generate_cfi () then begin
    emit (Cfi_def_cfa_offset bytes)
  end

let cfi_endproc () =
  if should_generate_cfi () then begin
    emit Cfi_endproc
  end

let cfi_offset ~reg ~offset =
  if should_generate_cfi () && offset <> 0 then begin
    emit (Cfi_offset { reg; offset; })
  end

let cfi_startproc () =
  if should_generate_cfi () then begin
    emit Cfi_startproc
  end

let comment text =
  if !Clflags.keep_asm_file then begin
    emit (Comment text)
  end

let loc ~file_num ~line ~col = emit_non_masm (Loc { file_num; line; col; })
let space ~bytes = emit (Space { bytes; })
let string ?comment str = emit (Bytes { str; comment; })

let global symbol = emit (Global (Asm_symbol.encode symbol))
let indirect_symbol symbol = emit (Indirect_symbol (Asm_symbol.encode symbol))
let private_extern symbol = emit (Private_extern (Asm_symbol.encode symbol))
let size symbol cst =
  emit (Size (Asm_symbol.encode symbol, (lower_expr cst)))
let type_ symbol ~type_ = emit (Type (Asm_symbol.encode symbol, type_))

let sleb128 ?comment i =
  emit (Sleb128 { constant = Directive.Constant.Signed_int i; comment; })

let uleb128 ?comment i =
  emit (Uleb128 { constant = Directive.Constant.Unsigned_int i; comment; })

let set var cst =
  emit (Set (var, lower_expr cst))

let const ?comment constant
      (width : Directive.Constant_with_width.width_in_bytes) =
  let constant = lower_expr constant in
  let constant = Directive.Constant_with_width.create constant width in
  emit (Const { constant; comment; })

let const_machine_width ?comment constant =
  match TS.machine_width () with
  | Thirty_two -> const ?comment constant Thirty_two
  | Sixty_four -> const ?comment constant Sixty_four

let float32 f =
  let comment =
    if !Clflags.keep_asm_file then Some (Printf.sprintf "%.12f" f)
    else None
  in
  let f_int32 = Int64.of_int32 (Int32.bits_of_float f) in
  const ?comment (Signed_int f_int32) Sixty_four

let float64_core f f_int64 =
  match TS.machine_width () with
  | Sixty_four ->
    let comment =
      if !Clflags.keep_asm_file then Some (Printf.sprintf "%.12g" f)
      else None
    in
    const ?comment (Signed_int f_int64) Sixty_four
  | Thirty_two ->
    let comment_lo =
      if !Clflags.keep_asm_file then
        Some (Printf.sprintf "low part of %.12g" f)
      else
        None
    in
    let comment_hi =
      if !Clflags.keep_asm_file then
        Some (Printf.sprintf "high part of %.12g" f)
      else
        None
    in
    let lo = Signed_int (Int64.logand f_int64 0xffff_ffffL) in
    let hi = Signed_int (Int64.shift_right_logical f_int64 32) in
    if big_endian () then begin
      const ?comment:comment_hi hi Thirty_two;
      const ?comment:comment_lo lo Thirty_two
    end else begin
      const ?comment:comment_lo lo Thirty_two;
      const ?comment:comment_hi hi Thirty_two
    end

let float64 f = float64_core f (Int64.bits_of_float f)
let float64_from_bits f = float64_core (Int64.float_of_bits f) f

let size ?size_of symbol =
  match TS.system () with
  | GNU | Linux | FreeBSD | NetBSD | OpenBSD | Generic_BSD ->
    let size_of =
      match size_of with
      | None -> symbol
      | Some size_of -> size_of
    in
    size size_of (Sub (This, Symbol symbol))
  | _ -> ()

let label ?comment label =
  const_machine_width ?comment (Label label)

let define_label label =
  let lbl_section = Asm_label.section label in
  let this_section =
    match !current_section_ref with
    | None -> not_initialized ()
    | Some this_section -> this_section
  in
  if not (Asm_section.equal lbl_section this_section) then begin
    Misc.fatal_errorf "Cannot define label %a intended for section %a \
        in section %a"
      Asm_label.print label
      Asm_section.print lbl_section
      Asm_section.print this_section
  end;
  let typ : Directive.thing_after_label =
    if current_section_is_text () then Code
    else Machine_width_data
  in
  emit (New_label (Asm_label.encode label, typ))

let new_line () =
  if !Clflags.keep_asm_file then begin
    emit New_line
  end

let sections_seen = ref []

let switch_to_section section =
  let first_occurrence =
    if List.mem section !sections_seen then false
    else begin
      sections_seen := section::!sections_seen;
      true
    end
  in
  current_section_ref := Some section;
  let ({ names; flags; args; } : Asm_section.flags_for_section) =
    Asm_section.flags section ~first_occurrence
  in
  if not first_occurrence then begin
    new_line ()
  end;
  emit (Section { names; flags; args; });
  if first_occurrence then begin
    define_label (Asm_label.for_section section)
  end

let switch_to_section_raw ~names ~flags ~args =
  emit (Section { names; flags; args; })

let text () = switch_to_section Asm_section.Text
let data () = switch_to_section Asm_section.Data

module Cached_string = struct
  type t = {
    section : Asm_section.t;
    str : string;
    comment : string option;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { section = section1; str = str1; comment = comment1; }
          { section = section2; str = str2; comment = comment2; } =
      let c = Asm_section.compare section1 section2 in
      if c <> 0 then c
      else
        let c = String.compare str1 str2 in
        if c <> 0 then c
        else
          match comment1, comment2 with
          | None, None -> 0
          | None, Some _ -> -1
          | Some _, None -> 1
          | Some comment1, Some comment2 -> String.compare comment1 comment2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t = Hashtbl.hash t

    let print _ _ = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

let cached_strings = ref Cached_string.Map.empty
let temp_var_counter = ref 0

let reset () =
  cached_strings := Cached_string.Map.empty;
  sections_seen := [];
  temp_var_counter := 0

let file ?file_num ~file_name () =
  emit_non_masm (File { file_num = file_num; filename = file_name; })

let initialize ~big_endian ~(emit : Directive.t -> unit) =
  big_endian_ref := Some big_endian;
  emit_ref := Some emit;
  reset ();
  begin match TS.assembler () with
  | MASM | MacOS -> ()
  | GAS_like ->
    (* Forward label references are illegal in gas.  Just put them in for
       all assemblers, they won't harm. *)
    List.iter (fun (section : Asm_section.t) ->
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
          end)
      (Asm_section.all_sections_in_order ())
  end;
  file ~file_name:"" ();  (* PR#7037 *)
  switch_to_section Asm_section.Text

let file ~file_num ~file_name = file ~file_num ~file_name ()

let check_symbol_for_definition_in_current_section symbol =
  let symbol_section = Asm_symbol.section symbol in
  let this_section =
    match !current_section_ref with
    | None -> not_initialized ()
    | Some this_section -> this_section
  in
  if not (Asm_section.equal symbol_section this_section) then begin
    Misc.fatal_errorf "Cannot define symbol %a intended for section %a \
        in section %a"
      Asm_symbol.print symbol
      Asm_section.print symbol_section
      Asm_section.print this_section
  end

let define_data_symbol symbol =
  check_symbol_for_definition_in_current_section symbol;
  emit (New_label (Asm_symbol.encode symbol, Machine_width_data));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_OBJECT"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

(* CR mshinwell: Rename to [define_text_symbol]? *)
let define_function_symbol symbol =
  check_symbol_for_definition_in_current_section symbol;
  (* CR mshinwell: This shouldn't be called "New_label" *)
  emit (New_label (Asm_symbol.encode symbol, Code));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_FUNC"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let symbol ?comment sym = const_machine_width ?comment (Symbol sym)

let symbol_plus_offset symbol ~offset_in_bytes =
  let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
  const_machine_width (
    Add (Symbol symbol, Signed_int offset_in_bytes))

let int8 ?comment i =
  const ?comment (Signed_int (Int64.of_int (Int8.to_int i))) Eight

let int16 ?comment i =
  const ?comment (Signed_int (Int64.of_int (Int16.to_int i))) Sixteen

let int32 ?comment i =
  const ?comment (Signed_int (Int64.of_int32 i)) Thirty_two

let int64 ?comment i =
  const ?comment (Signed_int i) Sixty_four

let uint8 ?comment i =
  const ?comment (Unsigned_int (Uint64.of_uint8 i)) Eight

let uint16 ?comment i =
  const ?comment (Unsigned_int (Uint64.of_uint16 i)) Sixteen

let uint32 ?comment i =
  const ?comment (Unsigned_int (Uint64.of_uint32 i)) Thirty_two

let uint64 ?comment i =
  const ?comment (Unsigned_int i) Sixty_four

let targetint ?comment n =
  match Targetint.repr n with
  | Int32 n -> int32 ?comment n
  | Int64 n -> int64 ?comment n

let cache_string ?comment section str =
  let cached : Cached_string.t = { section; str; comment; } in
  match Cached_string.Map.find cached !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Asm_label.create section in
    cached_strings := Cached_string.Map.add cached label !cached_strings;
    label

let emit_cached_strings () =
  Cached_string.Map.iter (fun { section; str; comment; } label_name ->
      switch_to_section section;
      define_label label_name;
      string ?comment str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := Cached_string.Map.empty

let mark_stack_non_executable () =
  let current_section = current_section () in
  match TS.system () with
  | Linux ->
    section ~names:[".note.GNU-stack"] ~flags:(Some "") ~args:["%progbits"];
    switch_to_section current_section
  | _ -> ()

let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "Ltemp%d" id

let force_assembly_time_constant section expr =
  match TS.assembler () with
  | GAS_like | MASM -> expr
  | MacOS ->
    (* This ensures the correct result is obtained on macOS.  (Apparently
       just writing expressions such as "L100 - L101" inline can cause
       unexpected results when one of the labels is on a section boundary,
       for example.) *)
    let temp = new_temp_var () in
    set temp expr;
    let compilation_unit = Compilation_unit.get_current_exn () in
    let sym =
      Asm_symbol.of_external_name_no_prefix section compilation_unit temp
    in
    Symbol sym  (* not really a symbol, but OK. *)

let check_symbol_in_current_unit sym =
  let sym_unit = Asm_symbol.compilation_unit sym in
  let this_unit = Compilation_unit.get_current_exn () in
  if not (Compilation_unit.equal sym_unit this_unit) then begin
    Misc.fatal_errorf "Symbol not in current compilation unit: %a"
      Asm_symbol.print sym
  end

let check_symbols_in_same_section sym1 sym2 =
  let sym1_section = Asm_symbol.section sym1 in
  let sym2_section = Asm_symbol.section sym2 in
  if not (Asm_section.equal sym1_section sym2_section) then begin
    Misc.fatal_errorf "Symbols %a (in section %a) and %a (in section %a) \
        are not in the same section"
      Asm_symbol.print sym1
      Asm_section.print sym1_section
      Asm_symbol.print sym2
      Asm_section.print sym2_section
  end

let check_labels_in_same_section lbl1 lbl2 =
  let lbl1_section = Asm_label.section lbl1 in
  let lbl2_section = Asm_label.section lbl2 in
  if not (Asm_section.equal lbl1_section lbl2_section) then begin
    Misc.fatal_errorf "Labels %a (in section %a) and %a (in section %a) \
        are not in the same section"
      Asm_label.print lbl1
      Asm_section.print lbl1_section
      Asm_label.print lbl2
      Asm_section.print lbl2_section
  end

let check_symbol_and_label_in_same_section sym lbl =
  let sym_section = Asm_symbol.section sym in
  let lbl_section = Asm_label.section lbl in
  if not (Asm_section.equal sym_section lbl_section) then begin
    Misc.fatal_errorf "Symbol %a (in section %a) and label %a (in section %a) \
        are not in the same section"
      Asm_symbol.print sym
      Asm_section.print sym_section
      Asm_label.print lbl
      Asm_section.print lbl_section
  end

let between_symbols_in_current_unit ~upper ~lower =
  check_symbol_in_current_unit upper;
  check_symbol_in_current_unit lower;
  check_symbols_in_same_section upper lower;
  let expr = Sub (Symbol upper, Symbol lower) in
  let section = Asm_symbol.section upper in
  const_machine_width (force_assembly_time_constant section expr)

let between_labels_16_bit ~upper ~lower =
  check_labels_in_same_section upper lower;
  let expr = Sub (Label upper, Label lower) in
  let section = Asm_label.section upper in
  const (force_assembly_time_constant section expr) Sixteen

let between_labels_32_bit ~upper ~lower =
  check_labels_in_same_section upper lower;
  let expr = Sub (Label upper, Label lower) in
  let section = Asm_label.section upper in
  const (force_assembly_time_constant section expr) Thirty_two

let between_labels_64_bit ~upper ~lower =
  check_labels_in_same_section upper lower;
  let expr = Sub (Label upper, Label lower) in
  let section = Asm_label.section upper in
  const (force_assembly_time_constant section expr) Sixty_four

let between_symbol_in_current_unit_and_label_offset ?comment
      ~upper ~lower ~offset_upper =
  check_symbol_in_current_unit lower;
  check_symbol_and_label_in_same_section lower upper;
  let section = Asm_symbol.section lower in
  if Targetint.compare offset_upper Targetint.zero = 0 then
    let expr = Sub (Label upper, Symbol lower) in
    const_machine_width ?comment (force_assembly_time_constant section expr)
  else
    let offset_upper = Targetint.to_int64 offset_upper in
    let expr =
      Sub (Add (Label upper, Signed_int offset_upper), Symbol lower)
    in
    const_machine_width ?comment (force_assembly_time_constant section expr)

let between_this_and_label_offset_32bit ~upper ~offset_upper =
  let upper_section = Asm_label.section upper in
  begin match !current_section_ref with
  | None -> not_initialized ()
  | Some this_section ->
    if not (Asm_section.equal upper_section this_section) then begin
      Misc.fatal_errorf "Label %a in section %a is not in the \
          current section %a"
        Asm_label.print upper
        Asm_section.print upper_section
        Asm_section.print this_section
    end
  end;
  let offset_upper = Targetint.to_int64 offset_upper in
  let expr =
    Sub (Add (Label upper, Signed_int offset_upper), This)
  in
  const (force_assembly_time_constant upper_section expr) Thirty_two

let offset_into_dwarf_section_label ?comment section upper
      ~(width : TS.machine_width) =
  let upper_section = Asm_label.section upper in
  let expected_section : Asm_section.t = DWARF section in
  if not (Asm_section.equal upper_section expected_section) then begin
    Misc.fatal_errorf "Label %a (in section %a) is not in section %a"
      Asm_label.print upper
      Asm_section.print upper_section
      Asm_section.print expected_section
  end;
  let expr : expr =
    match TS.assembler () with
    | MacOS ->
      let lower = Asm_label.for_dwarf_section section in
      force_assembly_time_constant expected_section
        (Sub (Label upper, Label lower))
    | GAS_like | MASM ->
      Label upper
  in
  let width : Directive.Constant_with_width.width_in_bytes =
    match width with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  let comment =
    if not !Clflags.keep_asm_file then None
    else
      match comment with
      | None ->
        Some (Format.asprintf "offset into %s"
          (Asm_section.to_string expected_section))
      | Some comment ->
        Some (Format.asprintf "%s (offset into %s)"
          comment (Asm_section.to_string expected_section))
  in
  const ?comment expr width

let offset_into_dwarf_section_symbol ?comment section upper
      ~(width : TS.machine_width) =
  let upper_section = Asm_symbol.section upper in
  if not (Asm_section.equal upper_section (DWARF section)) then begin
    Misc.fatal_errorf "Symbol %a (in section %a) not in section %a"
      Asm_symbol.print upper
      Asm_section.print upper_section
      Asm_section.print (Asm_section.DWARF section)
  end;
  let in_current_unit =
    Compilation_unit.equal (Compilation_unit.get_current_exn ())
      (Asm_symbol.compilation_unit upper)
  in
  (* The macOS assembler doesn't seem to allow "distance to undefined symbol
     from start of given section".  As such we make the assumption that
     the debug_info section will be relocated to start at zero; and reject any
     cross-unit reference that is not relative to the start of the debug_info
     section.  Relevant link:
     <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82005>.
  *)
  if not in_current_unit
    && not (Asm_section.equal (DWARF section) (DWARF Debug_info))
  then begin
    Misc.fatal_error "Don't know how to handle cross-unit references to \
      symbols that are not in the [Debug_info] section"
  end;
  let expr : expr = Symbol upper in
  let width : Directive.Constant_with_width.width_in_bytes =
    match width with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  let comment =
    if not !Clflags.keep_asm_file then None
    else
      match comment with
      | None ->
        Some (Format.asprintf "offset into %s"
          (Asm_section.to_string (DWARF section)))
      | Some comment ->
        Some (Format.asprintf "%s (offset into %s)"
          comment (Asm_section.to_string (DWARF section)))
  in
  const ?comment expr width
