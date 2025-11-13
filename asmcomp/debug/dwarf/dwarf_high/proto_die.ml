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

type attribute = {
  attr : Dwarf_attributes.t;
  value : Dwarf_value.t;
  form : Dwarf_form.t;
}

type t = {
  tag : Dwarf_tag.t;
  attributes : attribute list;
  children : t list;
  has_children : bool;
}

let create tag =
  { tag; attributes = []; children = []; has_children = false }

let add_attribute t attr =
  { t with attributes = attr :: t.attributes }

let add_child t child =
  { t with children = child :: t.children; has_children = true }

let add_children t children =
  if children = [] then t
  else { t with children = List.rev_append children t.children; has_children = true }

let set_has_children t has_children =
  { t with has_children }

let tag t = t.tag

let attributes t = List.rev t.attributes

let children t = List.rev t.children

let has_children t = t.has_children

let make_attribute ~attr ~value ~form =
  { attr; value; form }

(* Helper functions for common attributes *)

let with_name t name =
  add_attribute t {
    attr = DW_AT_name;
    value = String name;
    form = DW_FORM_string;  (* DWARF 5: inline strings *)
  }

let with_type t type_offset =
  add_attribute t {
    attr = DW_AT_type;
    value = Reference (Offset type_offset);
    form = DW_FORM_ref4;
  }

let with_byte_size t size =
  add_attribute t {
    attr = DW_AT_byte_size;
    value = Constant (Int size);
    form = DW_FORM_data1;
  }

let with_encoding t encoding =
  add_attribute t {
    attr = DW_AT_encoding;
    value = Constant (Int (Dwarf_encoding.to_code encoding));
    form = DW_FORM_data1;
  }

let with_location t loc =
  add_attribute t {
    attr = DW_AT_location;
    value = Expr_loc loc;
    form = DW_FORM_exprloc;
  }

let with_pc_range t ~start ~end_ =
  (* Convert Code_address.t to appropriate Dwarf_value.t *)
  let addr_value addr =
    match Code_address.absolute addr with
    | Some abs -> Dwarf_value.Address abs
    | None ->
        (* Must be a label - extract it *)
        match Code_address.label addr with
        | Some lbl -> Dwarf_value.Label_address lbl
        | None -> failwith "Code_address is neither absolute nor label"
  in
  let t = add_attribute t {
    attr = DW_AT_low_pc;
    value = addr_value start;
    form = DW_FORM_addr;
  } in
  add_attribute t {
    attr = DW_AT_high_pc;
    value = addr_value end_;
    form = DW_FORM_addr;
  }

let with_const_value t value =
  add_attribute t {
    attr = DW_AT_const_value;
    value = Constant (Int value);
    form = DW_FORM_sdata;
  }

let with_declaration t is_decl =
  add_attribute t {
    attr = DW_AT_declaration;
    value = Flag is_decl;
    form = DW_FORM_flag_present;
  }

let with_external t is_external =
  add_attribute t {
    attr = DW_AT_external;
    value = Flag is_external;
    form = DW_FORM_flag_present;
  }

let with_artificial t is_artificial =
  add_attribute t {
    attr = DW_AT_artificial;
    value = Flag is_artificial;
    form = DW_FORM_flag_present;
  }

(* High-level constructors *)

let create_variable ~name ?type_ref ?location ?(is_parameter=false) ?(is_artificial=false) () =
  let tag = if is_parameter then Dwarf_tag.DW_TAG_formal_parameter
            else Dwarf_tag.DW_TAG_variable in
  let die = create tag in
  let die = with_name die name in
  let die = match type_ref with
    | Some ref -> with_type die ref
    | None -> die
  in
  let die = match location with
    | Some loc -> with_location die loc
    | None -> die
  in
  let die = if is_artificial then with_artificial die true else die in
  die

let create_parameter ~name ?type_ref ?location () =
  create_variable ~name ?type_ref ?location ~is_parameter:true ()

(* Pretty printing *)

let print_attribute ppf attr =
  Format.fprintf ppf "  %s = %a"
    (Dwarf_attributes.to_string attr.attr)
    Dwarf_value.print attr.value

let print ppf t =
  Format.fprintf ppf "@[<v 2>%s" (Dwarf_tag.to_string t.tag);
  if t.attributes <> [] then begin
    Format.fprintf ppf "@,@[<v>";
    List.iter (fun attr ->
      Format.fprintf ppf "%a@," print_attribute attr
    ) (List.rev t.attributes);
    Format.fprintf ppf "@]"
  end;
  Format.fprintf ppf "@]"

let rec print_tree_indent ppf t indent =
  Format.fprintf ppf "%s%s" indent (Dwarf_tag.to_string t.tag);
  if t.attributes <> [] then begin
    Format.fprintf ppf "@,";
    List.iter (fun attr ->
      Format.fprintf ppf "%s  %s = %a@,"
        indent
        (Dwarf_attributes.to_string attr.attr)
        Dwarf_value.print attr.value
    ) (List.rev t.attributes)
  end;
  if t.children <> [] then begin
    List.iter (fun child ->
      Format.fprintf ppf "@,";
      print_tree_indent ppf child (indent ^ "  ")
    ) (List.rev t.children)
  end

let print_tree ppf t =
  Format.fprintf ppf "@[<v>";
  print_tree_indent ppf t "";
  Format.fprintf ppf "@]"
