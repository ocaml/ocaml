(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type position = {
  file: string;
  line: int;
  column: int;
}

type entry = {
  address: Code_address.t;
  position: position;
  is_stmt: bool;
  basic_block: bool;
  prologue_end: bool;
  epilogue_begin: bool;
}

type t = {
  mutable entries: entry list;
  mutable comp_dir: string;
  mutable file_names: string list;
}

let create () = {
  entries = [];
  comp_dir = "";
  file_names = [];
}

let add_entry t entry =
  t.entries <- entry :: t.entries;
  if not (List.mem entry.position.file t.file_names) then
    t.file_names <- entry.position.file :: t.file_names

let end_sequence _t =
  (* Sequences are tracked implicitly by entries *)
  ()

let files t = List.rev t.file_names

let set_comp_dir t dir =
  t.comp_dir <- dir

(* Line number program parameters (DWARF 4 standard defaults) *)
let minimum_instruction_length = 1
let maximum_operations_per_instruction = 1
let default_is_stmt = true
let line_base = -5
let line_range = 14
let opcode_base = 13

(* Calculate special opcode if possible *)
(* Generate opcodes for state machine *)
let generate_opcodes entries =
  let opcodes = ref [] in
  let emit op = opcodes := op :: !opcodes in

  (* State machine registers *)
  let file_map = Hashtbl.create 10 in
  let get_file_index file =
    match Hashtbl.find_opt file_map file with
    | Some idx -> idx
    | None ->
        let idx = Hashtbl.length file_map + 1 in
        Hashtbl.add file_map file idx;
        idx
  in

  let current_file = ref 0 in
  let current_line = ref 1 in
  let current_column = ref 0 in
  let current_is_stmt = ref default_is_stmt in
  let current_basic_block = ref false in

  (* Process entries in reverse order (they were added in reverse) *)
  let sorted_entries = List.rev entries in

  (* Emit initial address *)
  begin match sorted_entries with
  | [] -> ()
  | first :: _ ->
      emit (Line_number_opcode.Extended
        (Line_number_opcode.DW_LNE_set_address first.address))
  end;

  (* Generate opcodes for each entry.
     To avoid O(n²) behavior, iterate with explicit tracking of next entry
     instead of using List.length and List.nth in the loop. *)
  let rec process_entries entries =
    match entries with
    | [] -> ()
    | [entry] ->
        (* Last entry *)
        process_entry entry None
    | entry :: (next :: _ as rest) ->
        (* Not last entry - next one is available *)
        process_entry entry (Some next);
        process_entries rest

  and process_entry entry next_opt =
    let file_idx = get_file_index entry.position.file in
    let target_line = entry.position.line in
    let target_column = entry.position.column in

    (* Update file if needed *)
    if file_idx <> !current_file then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_set_file, Some file_idx));
      current_file := file_idx
    end;

    (* Update column if needed *)
    if target_column <> !current_column then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_set_column, Some target_column));
      current_column := target_column
    end;

    (* Update is_stmt if needed *)
    if entry.is_stmt <> !current_is_stmt then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_negate_stmt, None));
      current_is_stmt := entry.is_stmt
    end;

    (* Set basic_block flag if needed *)
    if entry.basic_block && not !current_basic_block then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_set_basic_block, None));
      current_basic_block := true
    end;

    (* Set prologue_end if needed *)
    if entry.prologue_end then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_set_prologue_end, None))
    end;

    (* Set epilogue_begin if needed *)
    if entry.epilogue_begin then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_set_epilogue_begin, None))
    end;

    (* Calculate line delta *)
    let line_delta = target_line - !current_line in

    (* For now, use simple approach: advance_line + copy *)
    (* TODO: Optimize with special opcodes *)
    if line_delta <> 0 then begin
      emit (Line_number_opcode.Standard
        (Line_number_opcode.DW_LNS_advance_line, Some line_delta));
      current_line := target_line
    end;

    (* Emit copy to add this row to the line table *)
    emit (Line_number_opcode.Standard
      (Line_number_opcode.DW_LNS_copy, None));
    current_basic_block := false;

    (* For next entry, set its address explicitly.
       Since we use symbolic Code_address labels rather than numeric offsets,
       we cannot calculate PC deltas at compile time. Instead, emit
       DW_LNE_set_address for each entry (except the first). *)
    match next_opt with
    | Some next_entry ->
        emit (Line_number_opcode.Extended
          (Line_number_opcode.DW_LNE_set_address next_entry.address))
    | None -> ()
  in
  process_entries sorted_entries;

  (* End sequence *)
  emit (Line_number_opcode.Extended Line_number_opcode.DW_LNE_end_sequence);

  List.rev !opcodes

let emit_header buf t =
  (* Build header content in a temporary buffer to calculate its length *)
  let content_buf = Buffer.create 256 in

  (* Line number program parameters *)
  Buffer.add_char content_buf (Char.chr minimum_instruction_length);
  Buffer.add_char content_buf (Char.chr maximum_operations_per_instruction);
  Buffer.add_char content_buf (if default_is_stmt then '\001' else '\000');
  Buffer.add_char content_buf (Char.chr (line_base land 0xFF));
  Buffer.add_char content_buf (Char.chr line_range);
  Buffer.add_char content_buf (Char.chr opcode_base);

  (* Standard opcode lengths (opcodes 1-12) *)
  let opcode_lengths = [0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1] in
  List.iter (fun len -> Buffer.add_char content_buf (Char.chr len)) opcode_lengths;

  (* Include directories table (just compilation directory) *)
  if t.comp_dir <> "" then begin
    Buffer.add_string content_buf t.comp_dir;
    Buffer.add_char content_buf '\000'
  end;
  Buffer.add_char content_buf '\000'; (* End of directory list *)

  (* File names table *)
  List.iter (fun file ->
    Buffer.add_string content_buf file;
    Buffer.add_char content_buf '\000';
    Buffer.add_char content_buf '\000'; (* dir_index = 0 *)
    Buffer.add_char content_buf '\000'; (* mtime = 0 *)
    Buffer.add_char content_buf '\000'  (* size = 0 *)
  ) (List.rev t.file_names);
  Buffer.add_char content_buf '\000'; (* End of file list *)

  (* Calculate header_length and write it *)
  let header_length = Buffer.length content_buf in
  for i = 0 to 3 do
    Buffer.add_char buf (Char.chr ((header_length lsr (i * 8)) land 0xff))
  done;

  (* Write the content *)
  Buffer.add_buffer buf content_buf

let emit address_size t =
  let buf = Buffer.create 4096 in
  let relocations = ref [] in

  (* Generate opcodes *)
  let opcodes = generate_opcodes t.entries in

  (* Encode all opcodes, tracking relocations for label-based addresses *)
  let program_buf = Buffer.create 2048 in
  List.iter (fun opcode ->
    (* Record current offset before encoding *)
    let base_offset = Buffer.length program_buf in

    (* Encode the opcode *)
    let opcode_bytes = Line_number_opcode.encode address_size opcode in
    Buffer.add_bytes program_buf opcode_bytes;

    (* Check if this was a DW_LNE_set_address with a label *)
    match opcode with
    | Line_number_opcode.Extended (Line_number_opcode.DW_LNE_set_address addr) ->
        begin match Code_address.absolute addr with
        | Some _ -> () (* Absolute address, no relocation needed *)
        | None ->
            (* Label-based address: record relocation
               The address bytes start at: base_offset + 1 (extended prefix) +
               uleb128 length (1 + address_size bytes, typically encodes as 1 byte for address_size <= 127) +
               1 (opcode byte) = base_offset + 3 for typical address sizes *)
            let length_bytes = Leb128.encode_uleb128 (1 + address_size) in
            let addr_offset = base_offset + 1 + Bytes.length length_bytes + 1 in
            let label = Code_address.to_string addr in
            relocations := (addr_offset, label) :: !relocations
        end
    | _ -> ()
  ) opcodes;
  let program_bytes = Buffer.contents program_buf in

  (* Calculate total length *)
  (* total_length = version(2) + header_length(4) + header + program *)
  (* For simplicity, we'll calculate header size by emitting it *)
  let header_buf = Buffer.create 512 in
  Buffer.add_string header_buf "\x04\x00"; (* version 4 *)
  emit_header header_buf t;
  let header_bytes = Buffer.contents header_buf in
  let header_size = String.length header_bytes in
  let program_size = String.length program_bytes in
  let total_length = header_size + program_size in

  (* Emit unit_length (4 bytes) *)
  for i = 0 to 3 do
    Buffer.add_char buf (Char.chr ((total_length lsr (i * 8)) land 0xff))
  done;

  (* Emit header and program *)
  Buffer.add_string buf header_bytes;
  Buffer.add_string buf program_bytes;

  (* Adjust relocation offsets to account for unit_length (4 bytes) *)
  let adjusted_relocs = List.map (fun (off, label) ->
    (off + 4 + header_size, label)
  ) (List.rev !relocations) in

  (Bytes.of_string (Buffer.contents buf), adjusted_relocs)

let print ppf t =
  Format.fprintf ppf "Line number table:@\n";
  Format.fprintf ppf "  Compilation dir: %s@\n" t.comp_dir;
  Format.fprintf ppf "  Files:@\n";
  List.iter (fun file ->
    Format.fprintf ppf "    %s@\n" file
  ) (List.rev t.file_names);
  Format.fprintf ppf "  Entries: %d@\n" (List.length t.entries)
