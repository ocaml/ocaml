(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module L = Lambda

(* CR mshinwell: Obtain constants by calling C functions so they definitely
   match the C code. *)

(* CR mshinwell: Make sure the story is completely straight with regards
   to %r13. *)

(* CR mshinwell: Revisit whether we should ditch the call site address for
   direct call points.  We could emit these into separate files at
   compile time.  This would reduce memory consumption by some amount and
   also reduce the amount of code at each call site. *)

let index_within_node = ref 2 (* Cf. [Node_num_header_words] in the runtime. *)
(* The [lazy]s are to ensure that we don't create [Ident.t]s at toplevel
   when not using Spacetime profiling.  (This could cause stamps to differ
   between bytecode and native .cmis when no .mli is present, e.g. arch.ml.) *)
let spacetime_node = ref (lazy (Cmm.Cvar (Ident.create "dummy")))
let spacetime_node_ident = ref (lazy (Ident.create "dummy"))
let current_function_label = ref ""
let direct_tail_call_point_indexes = ref []

let something_was_instrumented () =
  !index_within_node > 2

let next_index_within_node ~words_needed =
  let index = !index_within_node in
  index_within_node := !index_within_node + words_needed;
  index

let reset ~spacetime_node_ident:ident ~function_label =
  index_within_node := 2;
  spacetime_node := lazy (Cmm.Cvar ident);
  spacetime_node_ident := lazy ident;
  direct_tail_call_point_indexes := [];
  current_function_label := function_label

let code_for_function_prologue ~function_name =
  let node_hole = Ident.create "node_hole" in
  let node = Ident.create "node" in
  let new_node = Ident.create "new_node" in
  let must_allocate_node = Ident.create "must_allocate_node" in
  let is_new_node = Ident.create "is_new_node" in
  let open Cmm in
  let initialize_direct_tail_call_points_and_return_node =
    let new_node_encoded = Ident.create "new_node_encoded" in
    (* The callee node pointers within direct tail call points must initially
       point back at the start of the current node and be marked as per
       [Encode_tail_caller_node] in the runtime. *)
    let indexes = !direct_tail_call_point_indexes in
    let body =
      List.fold_left (fun init_code index ->
          (* Cf. [Direct_callee_node] in the runtime. *)
          let offset_in_bytes = index * Arch.size_addr in
          let place = Ident.create "tail_init" in
          let place_minus_one = Ident.create "tail_init_minus_one" in
          Clet (place,
            Cop (Caddi, [Cvar new_node; Cconst_int offset_in_bytes]),
            Clet (place_minus_one,
              Cop (Caddi,
                [Cvar new_node;
                Cconst_int (offset_in_bytes - Arch.size_addr)]),
              Csequence (
                Cop (Cstore (Word_int, L.Assignment),
                  [Cvar place; Cvar new_node_encoded]),
                Csequence (
                  Cop (Cstore (Word_int, L.Assignment),
                    [Cvar place_minus_one; Cconst_int 3]),
                  init_code)))))
        (Cvar new_node)
        indexes
    in
    match indexes with
    | [] -> body
    | _ ->
      Clet (new_node_encoded,
        (* Cf. [Encode_tail_caller_node] in the runtime. *)
        Cop (Cor, [Cvar new_node; Cconst_int 1]),
        body)
  in
  let pc = Ident.create "pc" in
  Clet (node_hole, Cop (Cspacetime_node_hole, []),
    Clet (node, Cop (Cload Word_int, [Cvar node_hole]),
      Clet (must_allocate_node, Cop (Cand, [Cvar node; Cconst_int 1]),
        Clet (pc, Cconst_symbol function_name,
        Cifthenelse (Cop (Ccmpi Ceq, [Cvar must_allocate_node; Cconst_int 1]),
          Clet (is_new_node,
            Cop (Cextcall ("caml_spacetime_allocate_node",
              [| Int |], false, Debuginfo.none),
              [Cconst_int (1 + !index_within_node);
               Cvar pc;
               Cvar node_hole;
              ]),
            Clet (new_node, Cop (Cload Word_int, [Cvar node_hole]),
              Cifthenelse (Cop (Ccmpi Ceq, [Cvar is_new_node; Cconst_int 0]),
                Cvar new_node,
                initialize_direct_tail_call_points_and_return_node))),
            Cvar node)))))

let code_for_blockheader ~value's_header ~node ~dbg =
  let existing_profinfo = Ident.create "existing_profinfo" in
  let profinfo = Ident.create "profinfo" in
  let address_of_profinfo = Ident.create "address_of_profinfo" in
  let index_within_node =
    (* "1 +" to skip the slot for the PC of the allocation point. *)
    1 + (next_index_within_node ~words_needed:2)
  in
  let offset_into_node = Arch.size_addr * index_within_node in
  let open Cmm in
  let generate_new_profinfo =
    (* This will generate a static branch to a function that should usually
       be in the cache, which hopefully gives a good code size/performance
       balance. *)
    Cop (Cextcall ("caml_spacetime_generate_profinfo", [| Int |],
        false, Debuginfo.none),
      [Cvar address_of_profinfo])
  in
  (* Check if we have already allocated a profinfo value for this allocation
     point with the current backtrace.  If so, use that value; if not,
     allocate a new one. *)
  Clet (address_of_profinfo,
    Cop (Caddi, [
      Cvar node;
      Cconst_int offset_into_node;
    ]),
    Clet (existing_profinfo, Cop (Cload Word_int, [Cvar address_of_profinfo]),
      Clet (profinfo,
        Cifthenelse (
          (* CR mshinwell: name constant *)
          Cop (Ccmpi Cne, [Cvar existing_profinfo; Cconst_pointer 1]),
          Cvar existing_profinfo,
          generate_new_profinfo),
        (* [profinfo] is already shifted by [PROFINFO_SHIFT].
           It also has the bottom bit set!  To avoid generating more code,
           we can adjust [value's_header] using a trick.  The effect is to
           "or" in the profinfo value to the higher bits, whilst preserving
           all remaining bits. *)
        let value's_header = Nativeint.logxor value's_header 1n in
        Cop (Cxor, [Cvar profinfo; Cconst_natint value's_header]))))

type callee =
  | Direct of string
  | Indirect of Cmm.expression

let code_for_call ~node ~callee ~is_tail ~label =
  (* We treat self recursive calls as tail calls to avoid blow-ups in the
     graph. *)
  let is_self_recursive_call =
    match callee with
    | Direct callee -> callee = !current_function_label
    | Indirect _ -> false
  in
  let is_tail = is_tail || is_self_recursive_call in
  let words_needed =
    match callee with
    | Direct _ -> 3  (* Cf. [Direct_num_fields] in the runtime. *)
    | Indirect _ -> 2  (* Cf. [Indirect_num_fields] in the runtime. *)
  in
  let index_within_node = next_index_within_node ~words_needed in
  begin match callee with
    (* If this is a direct tail call point, we need to note down its index,
       so the correct initialization code can be emitted in the prologue. *)
    | Direct _ when is_tail ->
      direct_tail_call_point_indexes :=
        (* "+2" to skip the call site and callee PC values. *)
        (index_within_node + 2)::!direct_tail_call_point_indexes
    | Direct _ | Indirect _ -> ()
  end;
  let place_within_node = Ident.create "place_within_node" in
  let open Cmm in
  let encode_pc pc =
    (* CR mshinwell: consider whether the encoding could be optimised to
       reduce the overhead here
       If the PC values are sufficiently aligned the shift shouldn't be
       needed, which is probably more satisfactory anyway.
       This means we need a way of forcing the alignment of Ilabel.
       The callee addresses should already be sufficiently aligned. *)
    (* Cf. [Encode_call_point_pc] in the runtime. *)
    Cop (Cor, [Cop (Clsl, [pc; Cconst_int 2]); Cconst_int 3])
  in
  let within_node ~index =
    Cop (Caddi, [node; Cconst_int (index * Arch.size_addr)])
  in
  Clet (place_within_node,
    within_node ~index:index_within_node,
    Csequence (
      (* The "call site" address coincides with the return address
         used for the frame descriptor.  (We insert frame descriptors
         even in the case of tail calls when using allocation
         profiling.) *)
      Cop (Cstore (Word_int, L.Assignment), [
        Cvar place_within_node;
        encode_pc (Cop (Caddress_of_label label, []))]),
      match callee with
      | Direct callee ->
        Csequence (
          Cop (Cstore (Word_int, L.Assignment), [
            Cop (Caddi, [Cvar place_within_node; Cconst_int Arch.size_addr]);
            encode_pc (Cconst_symbol callee);
          ]),
          Cop (Cspacetime_load_node_hole_ptr,
            [Cop (Caddi, [Cvar place_within_node;
              Cconst_int (2 * Arch.size_addr)])]))
      | Indirect callee ->
        let node_hole_ptr = Ident.create "node_hole_ptr" in
        let caller_node =
          if is_tail then node
          else Cconst_int 1  (* [Val_unit] *)
        in
        Clet (node_hole_ptr,
          Cop (Cextcall ("caml_spacetime_indirect_node_hole_ptr",
              [| Int |], false, Debuginfo.none),
            [callee; Cvar place_within_node; caller_node]),
          Cop (Cspacetime_load_node_hole_ptr, [Cvar node_hole_ptr]))))

class virtual instruction_selection = object (self)
  inherit Selectgen.selector_generic as super

  (* [disable_instrumentation] ensures that we don't try to instrument the
     instrumentation... *)
  val mutable disable_instrumentation = false

  method private instrument_direct_call ~env ~lbl ~is_tail ~label =
    let instrumentation =
      code_for_call
        ~node:(Lazy.force !spacetime_node)
        ~callee:(Direct lbl)
        ~is_tail
        ~label
    in
    match self#emit_expr env instrumentation with
    | None -> ()
    | Some _ -> assert false

  method private instrument_indirect_call ~env ~callee ~is_tail ~label =
    (* [callee] is a pseudoregister, so we have to bind it in the environment
       and reference the variable to which it is bound. *)
    let callee_ident = Ident.create "callee" in
    let env = Tbl.add callee_ident [| callee |] env in
    let instrumentation =
      code_for_call
        ~node:(Lazy.force !spacetime_node)
        ~callee:(Indirect (Cmm.Cvar callee_ident))
        ~is_tail
        ~label
    in
    match self#emit_expr env instrumentation with
    | None -> ()
    | Some _ -> assert false

  method private can_instrument () =
    Config.spacetime && not disable_instrumentation

  method about_to_emit_call env desc arg =
    if not (self#can_instrument ()) then None
    else
      let module M = Mach in
      match desc with
      | M.Iop (M.Icall_imm lbl) ->
        assert (Array.length arg = 0);
        let label = Cmm.new_label () in
        self#instrument_direct_call ~env ~lbl ~is_tail:false ~label;
        Some label
      | M.Iop M.Icall_ind ->
        let label = Cmm.new_label () in
        assert (Array.length arg = 1);
        self#instrument_indirect_call ~env ~callee:arg.(0)
          ~is_tail:false ~label;
        Some label
      | M.Iop (M.Itailcall_imm lbl) ->
        let label = Cmm.new_label () in
        assert (Array.length arg = 0);
        self#instrument_direct_call ~env ~lbl ~is_tail:true ~label;
        Some label
      | M.Iop M.Itailcall_ind ->
        let label = Cmm.new_label () in
        assert (Array.length arg = 1);
        self#instrument_indirect_call ~env ~callee:arg.(0)
          ~is_tail:true ~label;
        Some label
      | M.Iop (M.Iextcall (lbl, _)) ->
        let label = Cmm.new_label () in
        assert (Array.length arg = 0);
        self#instrument_direct_call ~env ~lbl ~is_tail:false ~label;
        Some label
      | _ -> None

  method private instrument_blockheader ~env ~value's_header ~dbg =
    let instrumentation =
      code_for_blockheader
        ~node:(Lazy.force !spacetime_node_ident)
        ~value's_header ~dbg
    in
    self#emit_expr env instrumentation

  method private emit_prologue f ~node ~env_after_main_prologue
        ~last_insn_of_main_prologue =
    (* We don't need the prologue unless we inserted some instrumentation.
       This corresponds to adding the prologue if the function contains one
       or more call or allocation points. *)
    if something_was_instrumented () then begin
      let prologue_cmm =
        code_for_function_prologue ~function_name:f.Cmm.fun_name
      in
      (* Splice the allocation prologue after the main prologue but before the
         function body.  Remember that [instr_seq] points at the last
         instruction (the list is in reverse order). *)
      let last_insn_of_body = instr_seq in
      let first_insn_of_body = ref Mach.dummy_instr in
      while not (instr_seq == last_insn_of_main_prologue) do
        first_insn_of_body := instr_seq;
        instr_seq <- instr_seq.Mach.next
      done;
      instr_seq <- last_insn_of_main_prologue;
      disable_instrumentation <- true;
      let node_temp_reg =
        match self#emit_expr env_after_main_prologue prologue_cmm with
        | None ->
          Misc.fatal_error "Spacetime prologue instruction \
              selection did not yield a destination register"
        | Some node_temp_reg -> node_temp_reg
      in
      disable_instrumentation <- false;
      let node_reg = Tbl.find node env_after_main_prologue in
      self#insert_moves node_temp_reg node_reg;
      if not (!first_insn_of_body == Mach.dummy_instr) then begin
        (!first_insn_of_body).Mach.next <- instr_seq;
        instr_seq <- last_insn_of_body
      end
    end

  method! emit_blockheader env n dbg =
    if self#can_instrument () then begin
      disable_instrumentation <- true;
      let result = self#instrument_blockheader ~env ~value's_header:n ~dbg in
      disable_instrumentation <- false;
      result
    end else begin
      super#emit_blockheader env n dbg
    end

  method! select_allocation words =
    if self#can_instrument () then begin
      (* Leave space for a direct call point.  We cannot easily insert any
         instrumentation code, so the fields are filled in instead by
         [caml_spacetime_caml_garbage_collection]. *)
      let index = !index_within_node in
      index_within_node := !index_within_node + 3;
      Mach.Ialloc { words; spacetime_index = index; }
    end else begin
      super#select_allocation words
    end

  method! select_allocation_args env =
    if self#can_instrument () then begin
      let regs = Tbl.find (Lazy.force !spacetime_node_ident) env in
      match regs with
      | [| reg |] -> [| reg |]
      | _ -> failwith "Expected one register only for spacetime_node_ident"
    end else begin
      super#select_allocation_args env
    end

  method select_checkbound () =
    (* This follows [select_allocation], above. *)
    if self#can_instrument () then begin
      let index = !index_within_node in
      index_within_node := !index_within_node + 3;
      Mach.Icheckbound { spacetime_index = index; }
    end else begin
      super#select_checkbound ()
    end

  method select_checkbound_extra_args () =
    if self#can_instrument () then begin
      (* This follows [select_allocation_args], above. *)
      [Cmm.Cvar (Lazy.force !spacetime_node_ident)]
    end else begin
      super#select_checkbound_extra_args ()
    end

  method! initial_env () =
    let env = super#initial_env () in
    if Config.spacetime then
      Tbl.add (Lazy.force !spacetime_node_ident)
        (self#regs_for Cmm.typ_int) env
    else
      env

  method! emit_fundecl f =
    if Config.spacetime then begin
      disable_instrumentation <- false;
      reset ~spacetime_node_ident:f.Cmm.fun_spacetime_node
        ~function_label:f.Cmm.fun_name
    end;
    super#emit_fundecl f

  method! after_body f ~env_after_prologue ~last_insn_of_prologue =
    if Config.spacetime then begin
      self#emit_prologue f ~node:f.Cmm.fun_spacetime_node
        ~env_after_main_prologue:env_after_prologue
        ~last_insn_of_main_prologue:last_insn_of_prologue
    end;
    super#after_body f ~env_after_prologue ~last_insn_of_prologue
end
