(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

type error =
  | Stack_frame_too_large of int

exception Error of error

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (Int.to_string n)

let emit_char c = output_char !output_channel c

let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_int32 n = emit_printf "0x%lx" n

let macosx = Config.system = "macosx"

let string_of_symbol s =
  let buf = Buffer.create (String.length s + 10) in
  if macosx then Buffer.add_char buf '_';
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        Buffer.add_char buf c
    | _ ->
      if c = Compilenv.symbol_separator then
        Buffer.add_char buf c
      else
        Printf.bprintf buf "%s%02x" Compilenv.escape_prefix
          (Char.code c)
  done;
  Buffer.contents buf

let emit_symbol s =
  output_string !output_channel (string_of_symbol s)

let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\%o" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\""

let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    emit_string directive;
    emit_string_literal s;
    emit_char '\n'
  end else begin
    let i = ref 0 in
    while !i < l do
      let n = Int.min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_char '\n';
      i := !i + n
    done
  end

let emit_bytes_directive directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then emit_string directive
     else emit_char ',';
     emit_int(Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin emit_char '\n'; pos := 0 end
   done;
   if !pos > 0 then emit_char '\n'

let emit_float64_directive directive x =
  emit_printf "\t%s\t0x%Lx\n" directive x

let emit_float64_split_directive directive x =
  let lo = Int64.logand x 0xFFFF_FFFFL
  and hi = Int64.shift_right_logical x 32 in
  emit_printf "\t%s\t0x%Lx, 0x%Lx\n"
    directive
    (if Arch.big_endian then hi else lo)
    (if Arch.big_endian then lo else hi)

let emit_float32_directive directive x =
  emit_printf "\t%s\t0x%lx\n" directive x

let emit_size_directive symbol =
  if Config.asm_size_type_directives then begin
    emit_string "\t.size\t";
    emit_symbol symbol;
    emit_string ", . - ";
    emit_symbol symbol;
    emit_char '\n'
  end

let emit_type_directive symbol ty =
  if Config.asm_size_type_directives then begin
    emit_string "\t.type\t";
    emit_symbol symbol;
    emit_string ", ";
    emit_string ty;
    emit_char '\n'
  end

let emit_nonexecstack_note () =
  if Config.with_nonexecstack_note then begin
    emit_string "\t.section .note.GNU-stack,\"\",%progbits\n"
  end

(* Record live pointers at call points *)

type frame_debuginfo =
  | Dbg_alloc of Debuginfo.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_debuginfo: frame_debuginfo }     (* Location, if any *)

let frame_descriptors = ref([] : frame_descr list)

let record_frame_descr ~label ~frame_size ~live_offset debuginfo =
  frame_descriptors :=
    { fd_lbl = label;
      fd_frame_size = frame_size;
      fd_live_offset = List.sort_uniq (-) live_offset;
      fd_debuginfo = debuginfo } :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
    efa_8: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

let emit_frames a =
  let filenames = Hashtbl.create 7 in
  let label_filename name =
    try
      Hashtbl.find filenames name
    with Not_found ->
      let lbl = Cmm.new_label () in
      Hashtbl.add filenames name lbl;
      lbl
  in
  let defnames = Hashtbl.create 7 in
  let label_defname filename defname loc =
    try
      snd (Hashtbl.find defnames (filename, defname, loc))
    with Not_found ->
      let file_lbl = label_filename filename in
      let def_lbl = Cmm.new_label () in
      Hashtbl.add defnames (filename, defname, loc) (file_lbl, def_lbl);
      def_lbl
  in
  let module Label_table =
    Hashtbl.Make (struct
      type t = bool * Debuginfo.t

      let equal ((rs1 : bool), dbg1) (rs2, dbg2) =
        rs1 = rs2 && Debuginfo.compare dbg1 dbg2 = 0

      let hash (rs, dbg) =
        Hashtbl.hash (rs, Debuginfo.hash dbg)
    end)
  in
  let debuginfos = Label_table.create 7 in
  let label_debuginfos rs dbg =
    let rdbg = List.rev dbg in
    let key = (rs, rdbg) in
    try Label_table.find debuginfos key
    with Not_found ->
      let lbl = Cmm.new_label () in
      Label_table.add debuginfos key lbl;
      lbl
  in
  let efa_16_checked n =
    assert (n >= 0);
    if n < 0x1_0000
    then a.efa_16 n
    else raise (Error(Stack_frame_too_large n))
  in
  let emit_frame fd =
    assert (fd.fd_frame_size land 3 = 0);
    let flags =
      match fd.fd_debuginfo with
      | Dbg_other d | Dbg_raise d ->
        if Debuginfo.is_none d then 0 else 1
      | Dbg_alloc dbgs ->
        if !Clflags.debug &&
           List.exists (fun d ->
             not (Debuginfo.is_none d.Debuginfo.alloc_dbg)) dbgs
        then 3 else 2
    in
    a.efa_code_label fd.fd_lbl;
    efa_16_checked (fd.fd_frame_size + flags);
    efa_16_checked (List.length fd.fd_live_offset);
    List.iter efa_16_checked fd.fd_live_offset;
    begin match fd.fd_debuginfo with
    | _ when flags = 0 ->
      ()
    | Dbg_other dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos false dbg) Int32.zero
    | Dbg_raise dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos true dbg) Int32.zero
    | Dbg_alloc dbg ->
      assert (List.length dbg < 256);
      a.efa_8 (List.length dbg);
      List.iter (fun Debuginfo.{alloc_words;_} ->
        (* Possible allocations range between 2 and 257 *)
        assert (2 <= alloc_words &&
                alloc_words - 1 <= Config.max_young_wosize &&
                Config.max_young_wosize <= 256);
        a.efa_8 (alloc_words - 2)) dbg;
      if flags = 3 then begin
        a.efa_align 4;
        List.iter (fun Debuginfo.{alloc_dbg; _} ->
          if Debuginfo.is_none alloc_dbg then
            a.efa_32 Int32.zero
          else
            a.efa_label_rel (label_debuginfos false alloc_dbg) Int32.zero) dbg
      end
    end;
    a.efa_align Arch.size_addr
  in
  let emit_filename name lbl =
    a.efa_def_label lbl;
    a.efa_string name
  in
  let emit_defname (_filename, defname, loc) (file_lbl, lbl) =
    let emit_loc (start_chr, end_chr, end_offset) =
      a.efa_16 start_chr;
      a.efa_16 end_chr;
      a.efa_32 (Int32.of_int end_offset)
    in
    (* These must be 32-bit aligned, both because they contain a
       32-bit value, and because emit_debuginfo assumes the low 2 bits
       of their addresses are 0. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    a.efa_label_rel file_lbl 0l;
    (* Include the additional 64-bits of location information which didn't pack
       in the main 64-bit word *)
    Option.iter emit_loc loc;
    a.efa_string defname
  in
  let fully_pack_info fd_raise d has_next =
    (* See format in caml_debuginfo_location in runtime/backtrace-nat.c *)
    let open Debuginfo in
    let kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0
    and char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
    let char_end_offset = d.dinfo_end_bol - d.dinfo_start_bol in
    Int64.(add (shift_left (of_int d.dinfo_line) 51)
             (add (shift_left (of_int (d.dinfo_end_line - d.dinfo_line)) 48)
                (add (shift_left (of_int d.dinfo_char_start) 42)
                   (add (shift_left (of_int char_end) 35)
                      (add (shift_left (of_int char_end_offset) 26)
                         (add (shift_left (of_int kind) 1)
                            (of_int has_next)))))))
  in
  let partially_pack_info fd_raise d has_next =
    (* Partially packed debuginfo:
       1lllllllllmmmmmmmmddddddddddddkn
         1           - d points to a name_and_loc_info struct
         l (19 bits) - start line number
         m (18 bits) - offset of end line number from start
         d (24 bits) - memory offset to name_and_loc_info struct
         k (1 bit)   - fd_raise flag
         n (1 bit)   - has_next flag *)
    let open Debuginfo in
    let start_line = Int.min 0x7FFFF d.dinfo_line
    and end_line = Int.min 0x3FFFF (d.dinfo_end_line - d.dinfo_line)
    and kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0 in
    Int64.(add (shift_left Int64.one 63)
             (add (shift_left (of_int start_line) 44)
                (add (shift_left (of_int end_line) 26)
                   (add (shift_left (of_int kind) 1)
                      (of_int has_next)))))
  in
  let emit_debuginfo (rs, rdbg) lbl =
    (* Due to inlined functions, a single debuginfo may have multiple locations.
       These are represented sequentially in memory (innermost frame first),
       with the low bit of the packed debuginfo being 0 on the last entry. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    let rec emit rs d rest =
      let open Debuginfo in
      let defname = Scoped_location.string_of_scopes d.dinfo_scopes in
      let char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
      let is_fully_packable =
        d.dinfo_line <= 0xFFF
        && d.dinfo_end_line - d.dinfo_line <= 0x7
        && d.dinfo_char_start <= 0x3F
        && char_end <= 0x7F
        && d.dinfo_end_bol - d.dinfo_start_bol <= 0x1FF
      in
      let info =
        if is_fully_packable then
          fully_pack_info rs d (rest <> [])
        else
          partially_pack_info rs d (rest <> [])
      in
      let loc =
        if is_fully_packable then
          None
        else
          Some (Int.min 0xFFFF d.dinfo_char_start,   (* start_chr *)
                Int.min 0xFFFF char_end,             (* end_chr *)
                Int.min 0x3FFFFFFF d.dinfo_char_end) (* end_offset *)
      in
      a.efa_label_rel
        (label_defname d.dinfo_file defname loc)
        (Int64.to_int32 info);
      a.efa_32 (Int64.to_int32 (Int64.shift_right info 32));
      match rest with
      | [] -> ()
      | d :: rest -> emit false d rest in
    match rdbg with
    | [] -> assert false
    | d :: rest -> emit rs d rest in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
  Hashtbl.iter emit_defname defnames;
  a.efa_align Arch.size_addr;
  frame_descriptors := []

(* Detection of functions that can be duplicated between a DLL and
   the main program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

(* CFI directives *)

let is_cfi_enabled () =
  Config.asm_cfi_supported

let cfi_startproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_startproc\n"

let cfi_endproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_endproc\n"

let cfi_remember_state () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_remember_state\n"

let cfi_restore_state () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_restore_state\n"

let cfi_adjust_cfa_offset n =
  if is_cfi_enabled () then
  begin
    emit_string "\t.cfi_adjust_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_def_cfa_offset n =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_def_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_offset ~reg ~offset =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_offset ";
    emit_int reg;
    emit_string ", ";
    emit_int offset;
    emit_string "\n"
  end

let cfi_def_cfa_register ~reg =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_def_cfa_register ";
    emit_int reg;
    emit_string "\n"
  end

(* Emit debug information *)

(* This assoc list is expected to be very short *)
let file_pos_nums =
  (ref [] : (string * int) list ref)

(* Number of files *)
let file_pos_num_cnt = ref 1

(* Reset debug state at beginning of asm file *)
let reset_debug_info () =
  file_pos_nums := [];
  file_pos_num_cnt := 1

(* We only display .file if the file has not been seen before. We
   display .loc for every instruction. *)
let emit_debug_info_gen dbg file_emitter loc_emitter =
  (* Skip .file/.loc directives when using DWARF - we generate our own .debug_line section *)
  if not (Dwarf_flags.is_dwarf_enabled ()) &&
     is_cfi_enabled () &&
    (!Clflags.debug || Config.with_frame_pointers) then begin
    match List.rev dbg with
    | [] -> ()
    | { Debuginfo.dinfo_line = line;
        dinfo_char_start = col;
        dinfo_file = file_name; } :: _ ->
      if line > 0 then begin (* PR#6243 *)
        let file_num =
          try List.assoc file_name !file_pos_nums
          with Not_found ->
            let file_num = !file_pos_num_cnt in
            incr file_pos_num_cnt;
            file_emitter ~file_num ~file_name;
            file_pos_nums := (file_name,file_num) :: !file_pos_nums;
            file_num in
        loc_emitter ~file_num ~line ~col;
      end
  end

let emit_debug_info dbg =
  emit_debug_info_gen dbg (fun ~file_num ~file_name ->
      emit_string "\t.file\t";
      emit_int file_num; emit_char '\t';
      emit_string_literal file_name; emit_char '\n';
    )
    (fun ~file_num ~line ~col:_ ->
       emit_string "\t.loc\t";
       emit_int file_num; emit_char '\t';
       emit_int line; emit_char '\n')

let reset () =
  reset_debug_info ();
  frame_descriptors := []

let binary_backend_available = ref false
let create_asm_file = ref true

let report_error_doc ppf = function
  | Stack_frame_too_large n ->
      Format_doc.fprintf ppf "stack frame too large (%d bytes)" n

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let mk_env f : Emitenv.per_function_env =
  {
    f;
    stack_offset = 0;
    call_gc_sites = [];
    bound_error_sites = [];
    bound_error_call = None;
    call_gc_label = 0;
    jumptables_lbl = None;
    jumptables = [];
    float_literals = [];
    int_literals = [];
  }

let emit_named_text_section func_name prefix_char =
  if !Clflags.function_sections then begin
    emit_string "\t.section .text.caml.";
    emit_symbol func_name;
    emit_char ',';
    emit_string_literal "ax";
    emit_char ',';
    emit_char prefix_char;
    emit_string "progbits\n";
  end
  else
    emit_string "\t.text\n"

(* DWARF debugging information support *)

module Dwarf_helpers = struct
  let dwarf_state = ref None

  let init ~source_file ~compilation_dir ~producer =
    if Dwarf_flags.is_dwarf_enabled () then begin
      (* Initialize architecture-specific DWARF register number mapping *)
      Arch_reg_mapping.set_mapper Dwarf_reg_map.to_dwarf_register;
      Arch_reg_mapping.set_frame_pointer_register Dwarf_reg_map.frame_pointer_dwarf_register;

      (* Warn if architecture doesn't have verified DWARF register mapping *)
      let supported_archs = ["amd64"; "arm64"] in
      if not (List.mem Config.architecture supported_archs) then
        Printf.eprintf "Warning: DWARF support for architecture '%s' uses default register mapping.\n\
                        Register numbers and frame pointer may be incorrect. Verified architectures: %s\n%!"
          Config.architecture (String.concat ", " supported_archs);

      let state = Dwarf.create ~source_file ~compilation_dir ~producer ~address_size:Arch.size_addr () in
      dwarf_state := Some state
    end

  let add_function ~name ~start_address ~end_address =
    match !dwarf_state with
    | None -> ()
    | Some state ->
        Dwarf.add_function state ~name ~start_address ~end_address

  let add_line_number ~address ~file ~line ~column =
    match !dwarf_state with
    | None -> ()
    | Some state ->
        Dwarf.add_line_number state ~address ~file ~line ~column

  let add_variable ~name ~location ~is_parameter =
    match !dwarf_state with
    | None -> ()
    | Some state ->
        Dwarf.add_variable state ~name ~location ~is_parameter

  let emit_section_bytes oc bytes =
    (* Emit bytes as .byte directives, 16 bytes per line *)
    let len = Bytes.length bytes in
    let rec emit_chunk offset =
      if offset < len then begin
        output_string oc "\t.byte ";
        let chunk_end = min (offset + 16) len in
        for i = offset to chunk_end - 1 do
          if i > offset then output_string oc ",";
          Printf.fprintf oc "0x%02x" (Char.code (Bytes.get bytes i))
        done;
        output_string oc "\n";
        emit_chunk chunk_end
      end
    in
    emit_chunk 0

  (* Combined relocation type for unified processing *)
  type combined_relocation =
    | Addr_reloc of Dwarf_world.relocation
    | Sec_offset_reloc of Dwarf_world.relocation  (* 4-byte section offset *)
    | Str_reloc of Dwarf_world.str_relocation

  let emit_section_bytes_with_both_relocs oc address_size bytes addr_relocs sec_offset_relocs str_relocs =
    (* Combine and sort all relocations by offset *)
    let combined =
      List.map (fun (r : Dwarf_world.relocation) -> (r.Dwarf_world.offset, Addr_reloc r)) addr_relocs @
      List.map (fun (r : Dwarf_world.relocation) -> (r.Dwarf_world.offset, Sec_offset_reloc r)) sec_offset_relocs @
      List.map (fun (r : Dwarf_world.str_relocation) -> (r.Dwarf_world.offset, Str_reloc r)) str_relocs
    in
    let sorted = List.sort (fun (o1, _) (o2, _) -> compare o1 o2) combined in

    let len = Bytes.length bytes in
    let rec emit_from offset relocs_remaining =
      match relocs_remaining with
      | [] ->
          (* No more relocations - emit remaining bytes *)
          if offset < len then begin
            let rec emit_chunk off =
              if off < len then begin
                output_string oc "\t.byte ";
                let chunk_end = min (off + 16) len in
                for i = off to chunk_end - 1 do
                  if i > off then output_string oc ",";
                  Printf.fprintf oc "0x%02x" (Char.code (Bytes.get bytes i))
                done;
                output_string oc "\n";
                emit_chunk chunk_end
              end
            in
            emit_chunk offset
          end
      | (reloc_offset, reloc) :: rest ->
          (* Emit bytes up to relocation *)
          if offset < reloc_offset then begin
            let rec emit_chunk off =
              if off < reloc_offset then begin
                output_string oc "\t.byte ";
                let chunk_end = min (off + 16) reloc_offset in
                for i = off to chunk_end - 1 do
                  if i > off then output_string oc ",";
                  Printf.fprintf oc "0x%02x" (Char.code (Bytes.get bytes i))
                done;
                output_string oc "\n";
                emit_chunk chunk_end
              end
            in
            emit_chunk offset
          end;
          (* Emit relocation based on type *)
          (match reloc with
           | Addr_reloc r ->
               (* Labels coming from [Code_address] are already escaped (and
                  prefixed with the Mach-O underscore when appropriate)
                  because they were created via [string_of_symbol].  Emit them
                  verbatim so that DWARF relocations reference the exact
                  symbols defined in the text section.  Re-escaping or
                  prepending another underscore would produce identifiers that
                  the assembler/linker cannot resolve. *)
               let symbol = r.Dwarf_world.label in
               (match address_size with
                | 4 -> Printf.fprintf oc "\t.long %s\n" symbol
                | 8 -> Printf.fprintf oc "\t.quad %s\n" symbol
                | _ -> failwith (Printf.sprintf "Unsupported address size: %d" address_size));
               emit_from (reloc_offset + address_size) rest
           | Sec_offset_reloc r ->
               (* Emit 4-byte section-relative offset for DW_AT_stmt_list.
                  For multi-object linking to work, we need the linker to adjust
                  these offsets when concatenating .debug_line sections.

                  CRITICAL: Only ELF linkers properly convert relocations in debug
                  sections to section-relative offsets. Mach-O and other platforms
                  use absolute relocations or assembly-time computation, both of
                  which break multi-object linking. *)
               let label = r.Dwarf_world.label in
               if Config.system = "linux" || Config.system = "gnu" then begin
                 (* ELF: Emit label relocation. The ELF linker will convert this to a
                    section-relative offset in the merged .debug_line section. *)
                 Printf.fprintf oc "\t.long %s\n" label
               end else if Config.system = "macosx" then begin
                 (* Mach-O: Use subtractor relocation with weak section base symbol.
                    This creates a pair of relocations (ARM64_RELOC_SUBTRACTOR +
                    ARM64_RELOC_UNSIGNED) that the linker resolves to the offset
                    from the start of the merged .debug_line section. *)
                 Printf.fprintf oc "\t.long %s - __debug_line_section_base\n" label
               end else begin
                 (* Other platforms: Emit section-relative offset computed at assembly time.
                    LIMITATION: This breaks multi-object linking because the offset
                    is computed relative to this .o file's Ldebug_line_start, not the final
                    merged section. Platforms need specific relocation support or weak symbols. *)
                 Printf.fprintf oc "\t.long %s - Ldebug_line_start\n" label
               end;
               emit_from (reloc_offset + 4) rest
           | Str_reloc r ->
               (* Emit string table offset as plain numeric value.
                  LIMITATION: This approach works for single-CU debugging but
                  breaks when linking multiple .o files together, because each CU's
                  .debug_info still points to offsets relative to its own .debug_str
                  section start, not the merged section. A proper fix would require
                  section-relative relocations, but those cause linker crashes on
                  macOS (Mach-O) - tested with both ld_prime and ld_classic.

                  Alternative solutions:
                  1. Use DW_FORM_string (inline strings) - simple, works everywhere
                  2. Upgrade to DWARF 5 with DW_FORM_strx - reduces relocations
                  3. Adopt Apple's debug map approach - macOS-specific *)
               let str_offset = r.Dwarf_world.str_offset in
               Printf.fprintf oc "\t.long %d\n" str_offset;
               emit_from (reloc_offset + 4) rest)
    in
    emit_from 0 sorted

  let emit_debug_str_with_labels oc bytes labels =
    (* Emit the .debug_str section with labels before each string.
       The bytes contain raw string data (null-terminated strings concatenated).
       The labels list contains (label, (string, offset)) tuples in order. *)
    output_string oc "Ldebug_str_start:\n";
    List.iter (fun (label, (str, offset)) ->
      Printf.fprintf oc "%s:\n" label;
      (* Emit the string bytes starting at the recorded offset *)
      let len = String.length str + 1 in  (* +1 for null terminator *)
      let str_bytes = Bytes.sub bytes offset len in
      emit_section_bytes oc str_bytes
    ) labels

  let emit_dwarf oc =
    match !dwarf_state with
    | None -> ()
    | Some state ->
        let sections = Dwarf.emit state in
        (* Emit DWARF sections to assembly output *)
        output_string oc "\n\t# DWARF debugging information\n";
        if Config.system = "macosx" then begin
          (* macOS Mach-O format with __DWARF segment *)
          output_string oc "\t.section __DWARF,__debug_info,regular,debug\n";
          emit_section_bytes_with_both_relocs oc Arch.size_addr sections.debug_info sections.debug_info_relocs sections.debug_info_sec_offset_relocs sections.debug_str_relocs;
          output_string oc "\t.section __DWARF,__debug_abbrev,regular,debug\n";
          emit_section_bytes oc sections.debug_abbrev;
          (* Emit .debug_str only if non-empty (DWARF 5 with DW_FORM_string doesn't need it) *)
          (if Bytes.length sections.debug_str > 0 then begin
            output_string oc "\t.section __DWARF,__debug_str,regular,debug\n";
            emit_debug_str_with_labels oc sections.debug_str sections.debug_str_labels
          end);
          (* DWARF 5: .debug_str_offsets section *)
          (match sections.debug_str_offsets with
           | Some (bytes, str_relocs) ->
               output_string oc "\t.section __DWARF,__debug_str_offsets,regular,debug\n";
               emit_section_bytes_with_both_relocs oc Arch.size_addr bytes [] [] str_relocs
           | None -> ());
          (* Optional sections *)
          (match sections.debug_line with
           | Some (bytes, relocs) ->
               output_string oc "\t.section __DWARF,__debug_line,regular,debug\n";
               (* Emit weak global symbol at section start for multi-object linking.
                  The linker will keep only one instance of this symbol (from the first .o),
                  allowing DW_AT_stmt_list offsets to be computed correctly via
                  subtractor relocations: .long label - __debug_line_section_base *)
               output_string oc "\t.weak_definition __debug_line_section_base\n";
               output_string oc "__debug_line_section_base:\n";
               (* Emit local label for assembly-time computation (unused on Mach-O) *)
               output_string oc "Ldebug_line_start:\n";
               (* Emit label for this CU's line table if present *)
               (match sections.line_table_label with
                | Some label -> Printf.fprintf oc "%s:\n" label
                | None -> ());
               emit_section_bytes_with_both_relocs oc Arch.size_addr bytes relocs [] []
           | None -> ());
          (match sections.debug_loc with
           | Some bytes ->
               output_string oc "\t.section __DWARF,__debug_loc,regular,debug\n";
               emit_section_bytes oc bytes
           | None -> ());
          (match sections.debug_ranges with
           | Some bytes ->
               output_string oc "\t.section __DWARF,__debug_ranges,regular,debug\n";
               emit_section_bytes oc bytes
           | None -> ())
        end else begin
          (* Linux ELF format with .debug_* sections *)
          output_string oc "\t.section .debug_info,\"\",@progbits\n";
          emit_section_bytes_with_both_relocs oc Arch.size_addr sections.debug_info sections.debug_info_relocs sections.debug_info_sec_offset_relocs sections.debug_str_relocs;
          output_string oc "\t.section .debug_abbrev,\"\",@progbits\n";
          emit_section_bytes oc sections.debug_abbrev;
          (* Emit .debug_str only if non-empty (DWARF 5 with DW_FORM_string doesn't need it) *)
          (if Bytes.length sections.debug_str > 0 then begin
            output_string oc "\t.section .debug_str,\"MS\",@progbits,1\n";
            emit_debug_str_with_labels oc sections.debug_str sections.debug_str_labels
          end);
          (* DWARF 5: .debug_str_offsets section *)
          (match sections.debug_str_offsets with
           | Some (bytes, str_relocs) ->
               output_string oc "\t.section .debug_str_offsets,\"\",@progbits\n";
               emit_section_bytes_with_both_relocs oc Arch.size_addr bytes [] [] str_relocs
           | None -> ());
          (* Optional sections *)
          (match sections.debug_line with
           | Some (bytes, relocs) ->
               output_string oc "\t.section .debug_line,\"\",@progbits\n";
               (* Emit section start label for computing offsets *)
               output_string oc "Ldebug_line_start:\n";
               (* Emit label for this CU's line table if present *)
               (match sections.line_table_label with
                | Some label -> Printf.fprintf oc "%s:\n" label
                | None -> ());
               emit_section_bytes_with_both_relocs oc Arch.size_addr bytes relocs [] []
           | None -> ());
          (match sections.debug_loc with
           | Some bytes ->
               output_string oc "\t.section .debug_loc,\"\",@progbits\n";
               emit_section_bytes oc bytes
           | None -> ());
          (match sections.debug_ranges with
           | Some bytes ->
               output_string oc "\t.section .debug_ranges,\"\",@progbits\n";
               emit_section_bytes oc bytes
           | None -> ())
        end;
        output_string oc "\n"

  let reset () =
    dwarf_state := None
end
