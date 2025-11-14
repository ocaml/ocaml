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

type scope = {
  start_address : Code_address.t;
  end_address : Code_address.t;
}

type location_kind =
  | Register of int
  | Stack_offset of int
  | Frame_offset of int
  | Optimized_away
  | Constant of int
  | Expression of Dwarf_operator.t list

type location = {
  scope : scope;
  kind : location_kind;
}

type variable = {
  name : string;
  type_ref : int option;
  locations : location list;
  is_parameter : bool;
  is_artificial : bool;
}

let create_variable ~name ?type_ref ?(is_parameter=false) ?(is_artificial=false) () =
  {
    name;
    type_ref;
    locations = [];
    is_parameter;
    is_artificial;
  }

let add_location var loc =
  { var with locations = loc :: var.locations }

let is_address_in_scope addr scope =
  (* Check if an address falls within a scope's range.
     For label-based addresses that we can't compare numerically,
     conservatively assume they're visible (return true). *)
  match Code_address.absolute addr,
        Code_address.absolute scope.start_address,
        Code_address.absolute scope.end_address with
  | Some a, Some s, Some e -> a >= s && a < e
  | _ -> true  (* Label-based addresses - conservatively assume visible *)

let location_at_address var addr =
  let rec find_location = function
    | [] -> None
    | loc :: rest ->
        if is_address_in_scope addr loc.scope then
          Some loc.kind
        else
          find_location rest
  in
  find_location var.locations

let is_visible_at var addr =
  match location_at_address var addr with
  | Some _ -> true
  | None -> false

let location_to_expression kind =
  let buf = Buffer.create 32 in
  begin match kind with
  | Register backend_reg_num ->
      (* Convert backend register number to DWARF register number using
         architecture-specific mapping. This is necessary because different
         architectures use different register numbering schemes in their
         backends, but must emit standard DWARF register numbers. *)
      let dwarf_reg_num = Arch_reg_mapping.to_dwarf_register backend_reg_num in
      (* DW_OP_reg0 through DW_OP_reg31 *)
      if dwarf_reg_num >= 0 && dwarf_reg_num <= 31 then
        Buffer.add_char buf (Char.chr (0x50 + dwarf_reg_num)) (* DW_OP_reg0 + N *)
      else begin
        Buffer.add_char buf '\x90'; (* DW_OP_regx *)
        let reg_bytes = Leb128.encode_uleb128 dwarf_reg_num in
        Buffer.add_bytes buf reg_bytes
      end

  | Stack_offset offset ->
      (* DW_OP_fbreg: offset from frame base *)
      Buffer.add_char buf '\x91'; (* DW_OP_fbreg *)
      let offset_bytes = Leb128.encode_sleb128 offset in
      Buffer.add_bytes buf offset_bytes

  | Frame_offset offset ->
      (* DW_OP_fbreg *)
      Buffer.add_char buf '\x91';
      let offset_bytes = Leb128.encode_sleb128 offset in
      Buffer.add_bytes buf offset_bytes

  | Optimized_away ->
      (* Empty location expression means optimized away *)
      ()

  | Constant value ->
      (* DW_OP_consts *)
      Buffer.add_char buf '\x11'; (* DW_OP_consts *)
      let value_bytes = Leb128.encode_sleb128 value in
      Buffer.add_bytes buf value_bytes

  | Expression ops ->
      (* Encode list of DWARF operators.
         Only a minimal subset is currently supported. *)
      List.iter (fun op ->
        let op_byte = match op with
          | Dwarf_operator.DW_OP_plus -> '\x22'
          | Dwarf_operator.DW_OP_minus -> '\x1c'
          | Dwarf_operator.DW_OP_deref -> '\x06'
          | _ ->
              (* Fail loudly on unsupported operators instead of emitting
                 a placeholder that would produce nonsense DWARF. *)
              failwith (Printf.sprintf
                "Unsupported DWARF operator in location expression: %s. \
                 Only DW_OP_plus, DW_OP_minus, and DW_OP_deref are currently supported."
                (Dwarf_operator.to_string op))
        in
        Buffer.add_char buf op_byte
      ) ops
  end;
  Bytes.of_string (Buffer.contents buf)

let print_location ppf loc =
  Format.fprintf ppf "@[<v 2>Location:@,";
  Format.fprintf ppf "Scope: %s - %s@,"
    (Code_address.to_string loc.scope.start_address)
    (Code_address.to_string loc.scope.end_address);
  Format.fprintf ppf "Kind: ";
  begin match loc.kind with
  | Register reg -> Format.fprintf ppf "Register %d" reg
  | Stack_offset off -> Format.fprintf ppf "Stack[%d]" off
  | Frame_offset off -> Format.fprintf ppf "Frame[%d]" off
  | Optimized_away -> Format.fprintf ppf "Optimized away"
  | Constant value -> Format.fprintf ppf "Constant %d" value
  | Expression ops -> Format.fprintf ppf "Expression (%d ops)" (List.length ops)
  end;
  Format.fprintf ppf "@]"

let print_variable ppf var =
  Format.fprintf ppf "@[<v 2>Variable: %s@," var.name;
  if var.is_parameter then Format.fprintf ppf "Parameter@,";
  if var.is_artificial then Format.fprintf ppf "Artificial@,";
  begin match var.type_ref with
  | Some ref -> Format.fprintf ppf "Type: DIE #%d@," ref
  | None -> ()
  end;
  Format.fprintf ppf "Locations: %d@," (List.length var.locations);
  List.iter (fun loc ->
    Format.fprintf ppf "%a@," print_location loc
  ) var.locations;
  Format.fprintf ppf "@]"
