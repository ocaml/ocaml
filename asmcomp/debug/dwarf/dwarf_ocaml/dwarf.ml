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

type t = {
  source_file : string;
  world : Dwarf_world.t;
  mutable current_function : Proto_die.t option;
}

let is_enabled () =
  (* Check if debugging is enabled and DWARF fidelity is set *)
  !Clflags.debug && Dwarf_flags.is_dwarf_enabled ()

let create ~source_file ~compilation_dir ~producer () =
  if not (is_enabled ()) then
    Misc.fatal_error "DWARF generation requested but not enabled";

  let world = Dwarf_world.create
    ~producer
    ~comp_dir:compilation_dir
    ~source_file
    ~language:Dwarf_language.ocaml
    ()
  in

  (* Add standard OCaml type DIEs (int, value, etc.)
     These will be the first DIEs after the compilation unit DIE *)
  let _type_offsets = Dwarf_world.add_standard_types world in

  { source_file; world; current_function = None }

let finalize_current_function t =
  (* Add the current function (with all its variables) to the world *)
  match t.current_function with
  | None -> ()
  | Some func_die ->
      Dwarf_world.add_die t.world func_die;
      t.current_function <- None

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

  (* Store as current function (don't add to world yet - we'll add variables first) *)
  t.current_function <- Some func_die

let add_variable t ~name ~(location : Variable_location.location) ~is_parameter =
  match t.current_function with
  | None ->
      (* No current function - ignore variable *)
      ()
  | Some func_die ->
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

      (* Add variable as child of function *)
      let func_die = Proto_die.add_child func_die var_die in
      t.current_function <- Some func_die

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
