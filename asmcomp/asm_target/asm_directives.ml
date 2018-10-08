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
    let unsupported name =
      Misc.fatal_errorf "Unsupported asm directive [%s] for MASM" name
    in
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
    | Section _ -> Misc.fatal_error "Unknown section name for MASM emitter"
    | Space { bytes; } -> bprintf buf "\tBYTE\t%d DUP (?)" bytes
    | New_label (label, Code) -> bprintf buf "%s:" label
    | New_label (label, Machine_width_data) ->
      begin match TS.machine_width () with
      | Thirty_two -> bprintf buf "%s LABEL DWORD" label
      | Sixty_four -> bprintf buf "%s LABEL QWORD" label
      end
    | Cfi_adjust_cfa_offset _ -> unsupported "Cfi_adjust_cfa_offset"
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
    | Direct_assignment _ -> unsupported "Direct_assignment"

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
  | Label of Asm_label.t
  | Symbol of Asm_symbol.t
  | Add of proto_constant * proto_constant
  | Sub of proto_constant * proto_constant
  | Div of proto_constant * int

let rec lower_proto_constant (cst : proto_constant) : Directive.Constant.t =
  match cst with
  | Signed_int n -> Signed_int n
  | This -> This
  | Label lbl -> Named_thing (Asm_label.encode lbl)
  | Symbol sym -> Named_thing (Asm_symbol.encode sym)
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

let global symbol = emit (Global (Asm_symbol.encode symbol))
let indirect_symbol symbol = emit (Indirect_symbol (Asm_symbol.encode symbol))
let private_extern symbol = emit (Private_extern (Asm_symbol.encode symbol))
let size symbol cst =
  emit (Size (Asm_symbol.encode symbol, (lower_proto_constant cst)))
let type_ symbol ~type_ = emit (Type (Asm_symbol.encode symbol, type_))

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
  match TS.machine_width () with
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
    if big_endian () then begin
      const ~comment:comment_hi hi Thirty_two;
      const ~comment:comment_lo lo Thirty_two
    end else begin
      const ~comment:comment_lo lo Thirty_two;
      const ~comment:comment_hi hi Thirty_two
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

let label label_name = const_machine_width (Label label_name)

let define_label label_name =
  let typ : Directive.thing_after_label =
    if current_section_is_text () then Code
    else Machine_width_data
  in
  emit (New_label (Asm_label.encode label_name, typ))

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
  emit (Section { names; flags; args; });
  if first_occurrence then begin
    define_label (Asm_section.label section)
  end

let switch_to_section_raw ~names ~flags ~args =
  emit (Section { names; flags; args; })

let text () = switch_to_section Asm_section.Text
let data () = switch_to_section Asm_section.Data

let cached_strings = ref ([] : (string * Asm_label.t) list)
let temp_var_counter = ref 0

let reset () =
  cached_strings := [];
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
      Asm_section.all_sections_in_order
  end;
  file ~file_name:"" ();  (* PR#7037 *)
  switch_to_section Asm_section.Text

let file ~file_num ~file_name = file ~file_num ~file_name ()

let define_data_symbol symbol =
  emit (New_label (Asm_symbol.encode symbol, Machine_width_data));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_OBJECT"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let define_function_symbol symbol =
  if not (current_section_is_text ()) then begin
    Misc.fatal_error "[define_function_symbol] can only be called when \
      emitting to a text section"
  end;
  emit (New_label (Asm_symbol.encode symbol, Code));
  begin match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ symbol ~type_:"STT_FUNC"
  | GAS_like, true | MacOS, _ | MASM, _ -> ()
  end

let symbol sym = const_machine_width (Symbol sym)

let symbol_plus_offset symbol ~offset_in_bytes =
  let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
  const_machine_width (
    Add (Symbol symbol, Signed_int offset_in_bytes))

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
    let sym = Asm_symbol.of_external_name temp in
    Symbol sym  (* not really a symbol, but OK (same below) *)
  | GAS_like | MASM ->
    expr

let between_symbols ~upper ~lower =
  let expr = Sub (Symbol upper, Symbol lower) in
  const_machine_width (force_relocatable expr)

let between_labels_32bit ~upper ~lower =
  let expr = Sub (Label upper, Label lower) in
  const (force_relocatable expr) Thirty_two

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let offset_upper = Targetint.to_int64 offset_upper in
  let expr =
    Sub (Add (Label upper, Signed_int offset_upper), Symbol lower)
  in
  const_machine_width (force_relocatable expr)

let between_symbol_and_label_offset' ~upper ~lower ~offset_lower =
  let offset_lower = Targetint.to_int64 offset_lower in
  let expr =
    Sub (Symbol upper, Add (Label lower, Signed_int offset_lower))
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

let offset_into_section_label section upper ~(width : TS.machine_width) =
  let lower = Asm_section.label section in
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
      let sym = Asm_symbol.of_external_name temp in
      Symbol sym
    | GAS_like | MASM ->
      Label upper
  in
  let width : Directive.Constant_with_width.width_in_bytes =
    match width with
    | Thirty_two -> Thirty_two
    | Sixty_four -> Sixty_four
  in
  const expr width

let offset_into_section_symbol section upper ~(width : TS.machine_width) =
  let lower = Asm_section.label section in
  let expr : proto_constant =
    (* The same thing as for [offset_into_section_label] applies here. *)
    match TS.assembler () with
    | MacOS ->
      let temp = new_temp_var () in
      direct_assignment temp (Sub (Symbol upper, Label lower));
      let sym = Asm_symbol.of_external_name temp in
      Symbol sym
    | GAS_like | MASM -> Symbol upper
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
    let label = Asm_label.create () in
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
  let current_section = current_section () in
  match TS.system () with
  | Linux ->
    section ~names:[".note.GNU-stack"] ~flags:(Some "") ~args:["%progbits"];
    switch_to_section current_section
  | _ -> ()
