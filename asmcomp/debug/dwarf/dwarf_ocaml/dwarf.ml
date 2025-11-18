(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type scope_context =
  | Function_scope of Proto_die.t
  | Lexical_block of Proto_die.t

type t = {
  source_file : string;
  world : Dwarf_world.t;
  mutable current_function : Proto_die.t option;
  mutable scope_stack : scope_context list;
      (* Stack of scopes for nested lexical blocks *)
}

let is_enabled () =
  (* Check if debugging is enabled and DWARF fidelity is set *)
  !Clflags.debug && Dwarf_flags.is_dwarf_enabled ()

let create ~source_file ~compilation_dir ~producer ~address_size () =
  if not (is_enabled ()) then
    Misc.fatal_error "DWARF generation requested but not enabled";

  (* TODO: Initialize architecture-specific register mapping.
     For now, Arch_reg_mapping uses a default identity mapping.
     The proper mapping can be set later from architecture-specific init code:
       Arch_reg_mapping.set_mapper Dwarf_reg_map.to_dwarf_register; *)

  let world = Dwarf_world.create
    ~producer
    ~comp_dir:compilation_dir
    ~source_file
    ~language:Dwarf_language.ocaml
    ~address_size
    ()
  in

  (* Add standard OCaml type DIEs (int, value, etc.)
     These will be the first DIEs after the compilation unit DIE *)
  let _type_offsets = Dwarf_world.add_standard_types world in

  { source_file; world; current_function = None; scope_stack = [] }

let finalize_current_function t =
  (* Add the current function (with all its variables) to the world *)
  match t.current_function with
  | None -> ()
  | Some func_die ->
      Dwarf_world.add_die t.world func_die;
      t.current_function <- None;
      t.scope_stack <- []

(** Create a DWARF expression for the frame base (frame pointer register).
    Returns a bytes buffer containing the appropriate DW_OP_reg* opcode.
    Uses the architecture-specific frame pointer DWARF register number
    set by the backend initialization code. *)
let create_frame_base_expression () =
  let frame_pointer_dwarf_reg = Arch_reg_mapping.get_frame_pointer_register () in
  let buf = Buffer.create 1 in
  if frame_pointer_dwarf_reg >= 0 && frame_pointer_dwarf_reg <= 31 then
    (* DW_OP_reg0 through DW_OP_reg31: opcode is 0x50 + register number *)
    Buffer.add_char buf (Char.chr (0x50 + frame_pointer_dwarf_reg))
  else begin
    (* DW_OP_regx for registers > 31: opcode 0x90 followed by ULEB128 reg number *)
    Buffer.add_char buf '\x90';
    let reg_bytes = Leb128.encode_uleb128 frame_pointer_dwarf_reg in
    Buffer.add_bytes buf reg_bytes
  end;
  Buffer.to_bytes buf

let add_function t ~name ~start_address ~end_address =
  (* Finalize any previous function first *)
  finalize_current_function t;

  (* Create a new function DIE *)
  let func_die = Proto_die.create Dwarf_tag.DW_TAG_subprogram in
  let func_die = Proto_die.with_name func_die name in
  let func_die = Proto_die.with_pc_range func_die ~start:start_address ~end_:end_address in
  let func_die = Proto_die.with_external func_die true in
  (* Link function to source file in line table (file index 1) *)
  let func_die = Proto_die.with_decl_file func_die 1 in
  (* Add frame base so DW_OP_fbreg in stack parameter locations works correctly *)
  let frame_base_expr = create_frame_base_expression () in
  let func_die = Proto_die.with_frame_base func_die frame_base_expr in

  (* Store as current function (don't add to world yet - we'll add variables first) *)
  t.current_function <- Some func_die;
  t.scope_stack <- [Function_scope func_die]

let add_lexical_block t ~start_address ~end_address =
  match t.scope_stack with
  | [] ->
      (* No current scope - ignore *)
      ()
  | _ ->
      (* Create a lexical block DIE *)
      let block_die = Proto_die.create Dwarf_tag.DW_TAG_lexical_block in
      let block_die = Proto_die.with_pc_range block_die ~start:start_address ~end_:end_address in
      (* Push onto scope stack *)
      t.scope_stack <- Lexical_block block_die :: t.scope_stack

let end_lexical_block t =
  match t.scope_stack with
  | [] | [Function_scope _] ->
      (* At function level or empty - nothing to pop *)
      ()
  | Lexical_block block_die :: parent_scope :: rest ->
      (* Pop the lexical block and add it to its parent *)
      (match parent_scope with
       | Function_scope parent_die | Lexical_block parent_die ->
           let parent_die = Proto_die.add_child parent_die block_die in
           let updated_parent = match parent_scope with
             | Function_scope _ -> Function_scope parent_die
             | Lexical_block _ -> Lexical_block parent_die
           in
           t.scope_stack <- updated_parent :: rest;
           (* Update current_function if parent was the function *)
           (match parent_scope with
            | Function_scope _ -> t.current_function <- Some parent_die
            | Lexical_block _ -> ()))
  | Lexical_block _ :: [] ->
      (* Lexical block without parent - shouldn't happen *)
      t.scope_stack <- []
  | Function_scope _ :: _ :: _ ->
      (* Function_scope followed by more scopes - invalid state, ignore *)
      ()

let add_variable t ~name ~(location : Variable_location.location) ~is_parameter =
  match t.scope_stack with
  | [] ->
      (* No current scope - ignore variable *)
      ()
  | current_scope :: _rest ->
      let parent_die = match current_scope with
        | Function_scope die | Lexical_block die -> die
      in
      (* Convert location to DWARF expression bytes *)
      let location_expr = Variable_location.location_to_expression location.kind in

      (* Create variable DIE with type reference to the "value" type.
         Get the actual offset from the stored type offsets. *)
      let type_offsets = Dwarf_world.get_type_offsets t.world in
      let var_die = Proto_die.create_variable
        ~name
        ~type_ref:type_offsets.ocaml_value
        ~location:location_expr
        ~is_parameter
        ()
      in

      (* Add variable as child of current scope *)
      let updated_die = Proto_die.add_child parent_die var_die in
      let updated_scope = match current_scope with
        | Function_scope _ -> Function_scope updated_die
        | Lexical_block _ -> Lexical_block updated_die
      in
      t.scope_stack <- updated_scope :: (match t.scope_stack with _ :: rest -> rest | [] -> []);
      (* Update current_function if we're at function level *)
      (match current_scope with
       | Function_scope _ -> t.current_function <- Some updated_die
       | Lexical_block _ -> ())

let add_line_number t ~address ~file ~line ~column =
  Dwarf_world.add_line_number_entry t.world
    ~address
    ~file
    ~line
    ~column

let emit t =
  (* Finalize any pending function *)
  finalize_current_function t;

  Dwarf_world.emit t.world

let world t = t.world

let print ppf t =
  Format.fprintf ppf "@[<v>DWARF for %s:@," t.source_file;
  Dwarf_world.print ppf t.world;
  Format.fprintf ppf "@]"
