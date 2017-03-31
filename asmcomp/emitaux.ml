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

let emit_frames () =
  let filenames = Hashtbl.create 7 in
  let label_filename name =
    try
      Hashtbl.find filenames name
    with Not_found ->
      let lbl = Cmm.new_label () in
      Hashtbl.add filenames name lbl;
      lbl
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
  let rec label_debuginfos rs rdbg =
    let key = (rs, rdbg) in
    try fst (Label_table.find debuginfos key)
    with Not_found ->
      let lbl = Cmm.new_label () in
      let next =
        match rdbg with
        | [] -> assert false
        | _ :: [] -> None
        | _ :: ((_ :: _) as rdbg') -> Some (label_debuginfos false rdbg')
      in
      Label_table.add debuginfos key (lbl, next);
      lbl
  in
  let emit_debuginfo_label rs rdbg =
    D.label (label_debuginfos rs rdbg)
  in
  let emit_frame fd =
    D.label fd.fd_lbl;
    D.int16 (if Debuginfo.is_none fd.fd_debuginfo
             then Int16.of_int_exn fd.fd_frame_size
             else Int16.of_int_exn (fd.fd_frame_size + 1));
    D.int16 (Int16.of_int_exn (List.length fd.fd_live_offset));
    List.iter (fun offset -> D.int16 (Int16.of_int_exn offset))
      fd.fd_live_offset;
    D.align ~bytes:Arch.size_addr;
    match List.rev fd.fd_debuginfo with
    | [] -> ()
    | _ :: _ as rdbg -> emit_debuginfo_label fd.fd_raise rdbg
  in
  let emit_filename name lbl =
    D.define_label lbl;
    D.string name;
    D.align ~bytes:Arch.size_addr
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
    D.align ~bytes:Arch.size_addr;
    D.define_label' lbl;
    let info = pack_info rs d in
    D.between_this_and_label_offset_32bit
      ~upper:(label_filename d.Debuginfo.dinfo_file)
      ~offset_upper:(Targetint.of_int32 (Int64.to_int32 info));
    D.int32 (Int64.to_int32 (Int64.shift_right info 32));
    begin match next with
    | Some next -> D.define_label next
    | None -> D.int64 0L
    end
  in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
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

(* We only emit .file if the file has not been seen before. We
   emit .loc for every instruction. *)
let emit_debug_info dbg file_emitter loc_emitter =
  if !Clflags.debug then begin
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
            D.file ~file_num ~file_name;
            file_pos_nums := (file_name,file_num) :: !file_pos_nums;
            file_num in
        D.loc ~file_num ~line ~col;
      end
  end

(* Emission of data *)

let emit_item = function
  | Cglobal_symbol s -> D.global s
  | Cdefine_symbol s -> add_def_symbol s; D.define_symbol' s
  | Cint8 n -> D.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> D.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> D.int32 (Nativeint.to_int32 n)
  | Cint n -> D.nativeint n
  | Csingle f -> D.float32 f
  | Cdouble f -> D.float64 f
  | Csymbol_address s -> add_used_symbol s; D.symbol' s
  | Cstring s -> D.string s
  | Cskip bytes -> if bytes > 0 then D.space ~bytes
  | Calign bytes -> D.align ~bytes

let data l =
  D.switch_to_section Data;
  D.align ~bytes:Targetint.size;
  List.iter emit_item l

let symbols_defined = ref StringSet.empty
let symbols_used = ref StringSet.empty

let add_def_symbol s = symbols_defined := StringSet.add s !symbols_defined
let add_used_symbol s = symbols_used := StringSet.add s !symbols_used

let emit_global_symbol s =
  let sym = Compilenv.make_symbol (Some s) in
  add_def_symbol sym;
  let sym = D.string_of_symbol sym in
  D.global sym;
  D.define_symbol' sym

let emit_global_symbol_with_size s ~f =
  let sym = Compilenv.make_symbol (Some s) in
  add_def_symbol sym;
  let sym = D.string_of_symbol sym in
  D.global sym;
  D.define_symbol' sym;
  f ();
  D.size sym

let begin_assembly () =
  reset_debug_info ();
  D.switch_to_section Data;
  emit_global_symbol "data_begin";
  D.switch_to_section Text;
  emit_global_symbol "code_begin";

(* Tradeoff between code size and code speed *)
let fastcode_flag = ref true

(* Name of current function *)
let function_name = ref ""

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0

let fundecl fundecl ~f ~alignment_in_bytes =
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := new_label ()
  D.switch_to_section Text;
  D.align ~bytes:alignment_in_bytes;
  match TS.system with
  | S_macosx
    when not !Clflags.output_c_object
      && is_generic_function fundecl.fun_name ->  (* PR#4690 *)
    D.private_extern fundecl.fun_name
  | _ ->
    D.global fundecl.fun_name
  end;
  D.define_function_symbol' fundecl.fun_name;
  emit_debug_info fundecl.fun_dbg;
  D.cfi_startproc ();
  f ();
  D.cfi_endproc ();
  D.size fundecl.fun_name

let emit_spacetime_shapes () =
  D.switch_to_section Data;
  D.align ~bytes:8;
  emit_global_symbol "spacetime_shapes";
  List.iter (fun fundecl ->
      begin match fundecl.fun_spacetime_shape with
      | None -> ()
      | Some shape ->
        let funsym = D.string_of_symbol fundecl.fun_name in
        D.comment ("Shape for " ^ funsym ^ ":");
        D.symbol' fundecl.fun_name;
        List.iter (fun (part_of_shape, label) ->
            let tag =
              match part_of_shape with
              | Direct_call_point _ -> 1
              | Indirect_call_point -> 2
              | Allocation_point -> 3
            in
            D.int64 (Int64.of_int tag);
            D.label label;
            begin match part_of_shape with
            | Direct_call_point { callee; } -> D.symbol' callee;
            | Indirect_call_point -> ()
            | Allocation_point -> ()
            end)
          shape;
          D.int64 0L
      end)
    !all_functions;
  D.int64 0L;
  D.comment "End of Spacetime shapes."

let end_assembly () =
  D.switch_to_section Text;
  begin match Target_system.system with
  | S_macosx ->
    (* suppress "ld warning: atom sorting error" *)
    I.nop ()
  | _ -> ()
  end;
  if Config.spacetime then begin
    emit_spacetime_shapes ()
  end;
  emit_global_symbol_with_size "frametable" ~f:(fun () ->
    emit_frames ());
  D.mark_stack_as_non_executable ();  (* PR#4564 *)
  emit_global_symbol "code_end";
  D.switch_to_section Data;
  emit_global_symbol "data_end";
  D.int32 0l

let reset () =
  reset_debug_info ();
  frame_descriptors := [];
  symbols_defined := StringSet.empty;
  symbols_used := StringSet.empty

let binary_backend_available = ref false
let create_asm_file = ref true
