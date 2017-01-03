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

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (string_of_int n)

let emit_char c = output_char !output_channel c

let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_int32 n = emit_printf "0x%lx" n

let emit_symbol esc s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
        Printf.fprintf !output_channel "%c%02x" esc (Char.code c)
  done

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
      let n = min (l - !i) 80 in
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

(* Record live pointers at call points *)

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_raise: bool;                     (* Is frame for a raise? *)
    fd_debuginfo: Debuginfo.t }         (* Location, if any *)

let frame_descriptors = ref([] : frame_descr list)

let record_frame_descr ~label ~frame_size ~live_offset ~raise_frame debuginfo =
  frame_descriptors :=
    { fd_lbl = label;
      fd_frame_size = frame_size;
      fd_live_offset = List.sort_uniq (-) live_offset;
      fd_raise = raise_frame;
      fd_debuginfo = debuginfo } :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
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
  let debuginfos = Hashtbl.create 7 in
  let rec label_debuginfos rs rdbg =
    let key = (rs, rdbg) in
    try fst (Hashtbl.find debuginfos key)
    with Not_found ->
      let lbl = Cmm.new_label () in
      let next =
        match rdbg with
        | [] -> assert false
        | _ :: [] -> None
        | _ :: ((_ :: _) as rdbg') -> Some (label_debuginfos false rdbg')
      in
      Hashtbl.add debuginfos key (lbl, next);
      lbl
  in
  let emit_debuginfo_label rs rdbg =
    a.efa_data_label (label_debuginfos rs rdbg)
  in
  let emit_frame fd =
    a.efa_code_label fd.fd_lbl;
    a.efa_16 (if Debuginfo.is_none fd.fd_debuginfo
              then fd.fd_frame_size
              else fd.fd_frame_size + 1);
    a.efa_16 (List.length fd.fd_live_offset);
    List.iter a.efa_16 fd.fd_live_offset;
    a.efa_align Arch.size_addr;
    match List.rev fd.fd_debuginfo with
    | [] -> ()
    | _ :: _ as rdbg -> emit_debuginfo_label fd.fd_raise rdbg
  in
  let emit_filename name lbl =
    a.efa_def_label lbl;
    a.efa_string name;
    a.efa_align Arch.size_addr
  in
  let pack_info fd_raise d =
    let line = min 0xFFFFF d.Debuginfo.dinfo_line
    and char_start = min 0xFF d.Debuginfo.dinfo_char_start
    and char_end = min 0x3FF d.Debuginfo.dinfo_char_end
    and kind = if fd_raise then 1 else 0 in
    Int64.(add (shift_left (of_int line) 44)
             (add (shift_left (of_int char_start) 36)
                (add (shift_left (of_int char_end) 26)
                   (of_int kind))))
  in
  let emit_debuginfo (rs, rdbg) (lbl,next) =
    let d = List.hd rdbg in
    a.efa_align Arch.size_addr;
    a.efa_def_label lbl;
    let info = pack_info rs d in
    a.efa_label_rel
      (label_filename d.Debuginfo.dinfo_file)
      (Int64.to_int32 info);
    a.efa_32 (Int64.to_int32 (Int64.shift_right info 32));
    begin match next with
    | Some next -> a.efa_data_label next
    | None -> a.efa_word 0
    end
  in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Hashtbl.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
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

let cfi_adjust_cfa_offset n =
  if is_cfi_enabled () then
  begin
    emit_string "\t.cfi_adjust_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_offset ~reg ~offset =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_offset ";
    emit_int reg;
    emit_string ", ";
    emit_int offset;
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

(* We only diplay .file if the file has not been seen before. We
   display .loc for every instruction. *)
let emit_debug_info_gen dbg file_emitter loc_emitter =
  if is_cfi_enabled () &&
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
