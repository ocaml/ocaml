(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-40"]

module D = Asm_directives
module L = Linkage_name
module LR = Linkage_name.With_reloc
module Int16 = Numbers.Int16

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (string_of_int n)

let emit_char c = output_char !output_channel c

let emit_targetint n = output_string !output_channel (Targetint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_int32 n = emit_printf "0x%lx" n

let emit_directive =
  let buf = Buffer.create 42 in
  fun dir ->
    Buffer.clear buf;
    D.Directive.print buf dir;
    emit_string (Buffer.contents buf);
    emit_string "\n"

(* Record live pointers at call points *)

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_raise: bool;                     (* Is frame for a raise? *)
    fd_debuginfo: Debuginfo.t }         (* Location, if any *)

let frame_descriptors = ref([] : frame_descr list)

let record_frame_label ~frame_size ~slot_offset ?label ~live ~raise_ dbg =
  let lbl =
    match label with
    | None -> Cmm.new_label()
    | Some label -> label
  in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | {typ = Val; loc = Reg r} ->
          live_offset := ((r lsl 1) + 1) :: !live_offset
      | {typ = Val; loc = Stack s} as reg ->
          live_offset := slot_offset s (Proc.register_class reg) :: !live_offset
      | {typ = Addr} as r ->
          Misc.fatal_error ("bad GC root " ^ Reg.name r)
      | _ -> ()
    )
    live;
  frame_descriptors :=
    { fd_lbl = lbl;
      fd_frame_size = frame_size ();
      fd_live_offset = List.sort_uniq (-) !live_offset;
      fd_raise = raise_;
      fd_debuginfo = dbg } :: !frame_descriptors;
  lbl

let record_frame ~frame_size ~slot_offset ?label ~live ~raise_ dbg =
  let lbl =
    record_frame_label ~frame_size ~slot_offset ?label ~live ~raise_ dbg
  in
  D.define_label lbl

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
    (* CR-soon mshinwell: There should be overflow checks here. *)
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
    D.string (name ^ "\000");
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
    D.define_label lbl;
    let info = pack_info rs d in
    let offset_upper =
      Targetint.logand (Targetint.of_int64 info)
        (Targetint.of_int64 0xffff_ffffL)  (* undo sign extension *)
    in
    D.between_this_and_label_offset_32bit
      ~upper:(label_filename d.Debuginfo.dinfo_file)
      ~offset_upper;
    D.int32 (Int64.to_int32 (Int64.shift_right info 32));
    begin match next with
    | Some next -> D.label next
    | None -> D.targetint Targetint.zero
    end
  in
  D.targetint (Targetint.of_int (List.length !frame_descriptors));
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
  frame_descriptors := []

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
let emit_debug_info dbg =
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

let symbols_defined = ref L.Set.empty
let symbols_used = ref L.Set.empty

let add_def_symbol s =
  symbols_defined := L.Set.add s !symbols_defined

let add_used_symbol s =
  symbols_used := L.Set.add s !symbols_used

let emit_global_data_symbol name =
  let sym = Compilenv.make_symbol (Some name) in
  add_def_symbol sym;
  D.global sym;
  D.define_data_symbol sym

let emit_global_data_symbol_with_size name ~f =
  let sym = Compilenv.make_symbol (Some name) in
  add_def_symbol sym;
  D.global sym;
  D.define_data_symbol sym;
  f ();
  D.size sym

let all_functions = ref []

(* Tradeoff between code size and code speed *)
let fastcode_flag = ref true

(* Name of current function *)
let function_name = ref (Linkage_name.create "")

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref 0

(* Pending floating-point constants *)
let float_constants = ref ([] : (Int64.t * Cmm.label) list)

let num_float_constants () = List.length !float_constants

(* Pending integer constants *)
let int_constants = ref ([] : (Targetint.t * int) list)

(* Total space (in words) occupied by pending integer and FP literals *)
let size_constants = ref 0

(* Label a floating-point constant *)
let float_constant f =
  try
    List.assoc f !float_constants
  with Not_found ->
    let lbl = Cmm.new_label() in
    float_constants := (f, lbl) :: !float_constants;
    size_constants := !size_constants + (64 / Targetint.size);
    lbl

(* Emit all pending floating-point constants *)
let emit_float_constants ~in_current_section =
  if !float_constants <> [] then begin
    if not in_current_section then D.switch_to_section Eight_byte_literals;
    D.align ~bytes:8;
    List.iter
      (fun (f, lbl) ->
        D.define_label lbl;
        D.float64_from_bits f)
      !float_constants;
    float_constants := []
  end

(* Label an integer constant *)
let int_constant n =
  try
    List.assoc n !int_constants
  with Not_found ->
    let lbl = Cmm.new_label() in
    int_constants := (n, lbl) :: !int_constants;
    size_constants := !size_constants + (64 / Targetint.size);
    lbl

(* Emit all pending integer constants *)
let emit_int_constants ~in_current_section =
  if !int_constants <> [] then begin
    if not in_current_section then D.switch_to_section Eight_byte_literals;
    D.align ~bytes:8;
    List.iter
      (fun (n, lbl) ->
        D.define_label lbl;
        D.targetint n)
      !int_constants;
    int_constants := []
  end

let emit_constants ~in_current_section =
  emit_float_constants ~in_current_section;
  emit_int_constants ~in_current_section;
  size_constants := 0

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: Cmm.label;
    gc_return_lbl: Cmm.label;
    gc_frame: Cmm.label;
    stack_offset : int;
  }

let call_gc_sites = ref ([] : gc_call list)

let record_call_gc_site ~label:gc_lbl ~return_label:gc_return_lbl
      ~frame_label:gc_frame ~stack_offset =
  call_gc_sites :=
    { gc_lbl; gc_return_lbl; gc_frame; stack_offset; } :: !call_gc_sites

let emit_call_gc gc ~spacetime_before_uninstrumented_call ~emit_call
      ~emit_jump_to_label =
  D.define_label gc.gc_lbl;
  D.cfi_adjust_cfa_offset ~bytes:gc.stack_offset;
  spacetime_before_uninstrumented_call gc.gc_lbl;
  (* CR mshinwell: .s file formatting of this call is a bit crappy *)
  emit_call L.caml_call_gc;
  D.define_label gc.gc_frame;
  emit_jump_to_label gc.gc_return_lbl;
  D.cfi_adjust_cfa_offset ~bytes:(-gc.stack_offset)

(* Record calls to caml_ml_array_bound_error.
   In -g mode, or when using Spacetime profiling, we maintain one call to
   caml_ml_array_bound_error per bound check site.  Without -g, we can share
   a single call. *)

type bound_error_call =
  { bd_lbl: Cmm.label;                      (* Entry label *)
    bd_frame: Cmm.label;                    (* Label of frame descriptor *)
    stack_offset : int;
  }

let bound_error_sites = ref ([] : bound_error_call list)
let bound_error_call = ref 0

let bound_error_label ~frame_size ~slot_offset ?label dbg ~stack_offset =
  if !Clflags.debug || Config.spacetime then begin
    let lbl_bound_error = Cmm.new_label() in
    let lbl_frame =
      record_frame_label ~frame_size ~slot_offset
        ?label ~live:Reg.Set.empty ~raise_:false dbg
    in
    bound_error_sites :=
      { bd_lbl = lbl_bound_error; bd_frame = lbl_frame; stack_offset; }
        :: !bound_error_sites;
    lbl_bound_error
  end else begin
    if !bound_error_call = 0 then bound_error_call := Cmm.new_label();
    !bound_error_call
  end

let emit_call_bound_error bd ~emit_call
      ~spacetime_before_uninstrumented_call =
  D.define_label bd.bd_lbl;
  D.cfi_adjust_cfa_offset ~bytes:bd.stack_offset;
  spacetime_before_uninstrumented_call bd.bd_lbl;
  emit_call L.caml_ml_array_bound_error;
  D.define_label bd.bd_frame;
  D.cfi_adjust_cfa_offset ~bytes:(-bd.stack_offset)

let emit_call_bound_errors ~emit_call ~spacetime_before_uninstrumented_call =
  List.iter (fun bd ->
      emit_call_bound_error bd ~emit_call
        ~spacetime_before_uninstrumented_call)
    !bound_error_sites;
  if !bound_error_call > 0 then begin
    (* There might not be a unique offset from the CFA if there is only a
       single check-bound point.  We just have to put up with this. *)
    D.define_label !bound_error_call;
    emit_call L.caml_ml_array_bound_error
  end

let begin_assembly ?(code_section = D.Text) () =
  reset_debug_info ();
  all_functions := [];
  D.switch_to_section Data;
  emit_global_data_symbol "data_begin";
  D.switch_to_section code_section;
  emit_global_data_symbol "code_begin"

let fundecl ?branch_relaxation ?after_body ?alternative_name
      (fundecl : Linearize.fundecl)
      ~prepare ~emit_all ~alignment_in_bytes ~emit_call ~emit_jump_to_label
      ~spacetime_before_uninstrumented_call ~emit_numeric_constants =
  all_functions := fundecl :: !all_functions;
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := Cmm.new_label ();
  call_gc_sites := [];
  bound_error_sites := [];
  bound_error_call := 0;
  D.switch_to_section Text;
  D.align ~bytes:alignment_in_bytes;
  begin match alternative_name, Target_system.system () with
  | Some _alternative_name, _ -> ()
  | _, MacOS_like
    when not !Clflags.output_c_object
      && Linkage_name.is_generic_function fundecl.fun_name ->  (* PR#4690 *)
    D.private_extern fundecl.fun_name
  | _, _ ->
    D.global fundecl.fun_name
  end;
  let fun_symbol =
    match alternative_name with
    | None -> fundecl.fun_name
    | Some alternative_name -> alternative_name
  in
  D.define_function_symbol fun_symbol;
  add_def_symbol fun_symbol;
  emit_debug_info fundecl.fun_dbg;
  D.cfi_startproc ();
  prepare fundecl;
  begin match branch_relaxation with
  | None -> ()
  | Some (branch_relaxation, max_out_of_line_code_offset) ->
    let module BR = (val branch_relaxation : Branch_relaxation.S) in
    BR.relax fundecl.fun_body ~max_out_of_line_code_offset
  end;
  D.define_label !tailrec_entry_point;
  let cfi_offset = emit_all ~fun_body:fundecl.fun_body in
  D.cfi_adjust_cfa_offset ~bytes:(-cfi_offset);
  begin match after_body with
  | None -> ()
  | Some f -> f ()
  end;
  List.iter (fun gc ->
      emit_call_gc gc ~spacetime_before_uninstrumented_call ~emit_call
        ~emit_jump_to_label)
    !call_gc_sites;
  emit_call_bound_errors ~emit_call ~spacetime_before_uninstrumented_call;
  if emit_numeric_constants then begin
    emit_constants ~in_current_section:true
  end;
  D.switch_to_section Text;
  D.cfi_endproc ();
  D.size ~size_of:fundecl.fun_name fun_symbol

let emit_spacetime_shapes () =
  if Targetint.size <> 64 then begin
    Misc.fatal_error "Compiler is configured for Spacetime on a non-64-bit \
      target"
  end;
  D.switch_to_section Data;
  D.align ~bytes:8;
  emit_global_data_symbol "spacetime_shapes";
  List.iter (fun (fundecl : Linearize.fundecl) ->
      begin match fundecl.fun_spacetime_shape with
      | None -> ()
      | Some shape ->
        let funsym = L.name fundecl.fun_name in
        D.comment ("Shape for " ^ funsym ^ ":");
        D.symbol (LR.no_reloc fundecl.fun_name);
        List.iter
          (fun ((part_of_shape : Mach.spacetime_part_of_shape), label) ->
            let tag =
              match part_of_shape with
              | Direct_call_point _ -> 1
              | Indirect_call_point -> 2
              | Allocation_point -> 3
            in
            D.int64 (Int64.of_int tag);
            D.label label;
            begin match part_of_shape with
            | Direct_call_point { callee; } ->
              D.symbol (LR.no_reloc callee);
            | Indirect_call_point -> ()
            | Allocation_point -> ()
            end)
          shape;
        D.int64 0L
      end)
    !all_functions;
  D.int64 0L;
  D.comment "End of Spacetime shapes."

let reset () =
  reset_debug_info ();
  frame_descriptors := [];
  symbols_defined := L.Set.empty;
  symbols_used := L.Set.empty;
  size_constants := 0

let end_assembly ?(code_section = D.Text) ~emit_numeric_constants () =
  reset ();  (* XXX check this fixes msvc problem *)
  if Config.spacetime then begin
    emit_spacetime_shapes ()
  end;
  D.switch_to_section Data;
  D.align ~bytes:8;  (* PR#7591 *)
  emit_global_data_symbol_with_size "frametable" ~f:(fun () ->
    emit_frames ());
  if emit_numeric_constants then begin
    emit_constants ~in_current_section:false
  end;
  D.mark_stack_non_executable ();  (* PR#4564 *)
  D.switch_to_section code_section;
  emit_global_data_symbol "code_end";
  D.int32 0l;
  D.int32 0l;
  D.switch_to_section Data;
  emit_global_data_symbol "data_end";
  D.int32 0l;
  D.int32 0l

(* Emission of data *)

let emit_item (item : Cmm.data_item) =
  match item with
  | Cglobal_symbol s -> D.global s
  | Cdefine_symbol s -> add_def_symbol s; D.define_data_symbol s
  | Cint8 n -> D.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> D.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> D.int32 (Nativeint.to_int32 n)
  | Cint n -> D.targetint (Targetint.of_nativeint_exn n)
  | Csingle f -> D.float32 f
  | Cdouble f -> D.float64 f
  | Csymbol_address s -> add_used_symbol s; D.symbol (LR.no_reloc s)
  | Cstring s -> D.string s
  | Cskip bytes -> if bytes > 0 then D.space ~bytes
  | Calign bytes ->
    if bytes < Targetint.size then D.align ~bytes:(Targetint.size / 8)
    else D.align ~bytes

let data l =
  D.switch_to_section Data;
  D.align ~bytes:(Targetint.size / 8);
  List.iter emit_item l

let binary_backend_available = ref false
let create_asm_file = ref true

let symbols_defined () = !symbols_defined
let symbols_used () = !symbols_used
