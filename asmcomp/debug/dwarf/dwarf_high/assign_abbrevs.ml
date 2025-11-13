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

type abbrev_entry = {
  code : int;
  tag : Dwarf_tag.t;
  has_children : bool;
  attributes : (Dwarf_attributes.t * Dwarf_form.t) list;
}

type abbrev_table = {
  entries : abbrev_entry list;
}

type abbrev_die = {
  abbrev_code : int;
  proto_die : Proto_die.t;
}

(* Signature of a DIE for abbreviation matching *)
module Signature = struct
  type t = {
    tag : Dwarf_tag.t;
    has_children : bool;
    attr_forms : (Dwarf_attributes.t * Dwarf_form.t) list;
  }

  let of_proto_die (die : Proto_die.t) =
    let attrs = Proto_die.attributes die in
    let attr_forms = List.map (fun (attr : Proto_die.attribute) ->
      (attr.attr, attr.form)
    ) attrs in
    {
      tag = Proto_die.tag die;
      has_children = Proto_die.has_children die;
      attr_forms;
    }
end

(* Mutable state for building abbreviation table *)
type abbrev_state = {
  mutable next_code : int;
  mutable signatures : (Signature.t * int) list;
  mutable entries : abbrev_entry list;
}

let create_state () =
  { next_code = 1; signatures = []; entries = [] }

let find_or_create_abbrev state signature =
  match List.assoc_opt signature state.signatures with
  | Some code -> code
  | None ->
      let code = state.next_code in
      state.next_code <- code + 1;
      state.signatures <- (signature, code) :: state.signatures;
      let entry = {
        code;
        tag = signature.Signature.tag;
        has_children = signature.Signature.has_children;
        attributes = signature.Signature.attr_forms;
      } in
      state.entries <- entry :: state.entries;
      code

let assign_single state proto_die =
  let signature = Signature.of_proto_die proto_die in
  let abbrev_code = find_or_create_abbrev state signature in
  { abbrev_code; proto_die }

let rec assign_tree state proto_die =
  let abbrev_die = assign_single state proto_die in
  (* Process children *)
  let children = Proto_die.children proto_die in
  List.iter (fun child -> ignore (assign_tree state child)) children;
  abbrev_die

let assign proto_die =
  let state = create_state () in
  let abbrev_die = assign_tree state proto_die in
  let table = { entries = List.rev state.entries } in
  (abbrev_die, table)

let assign_multi proto_dies =
  let state = create_state () in
  let abbrev_dies = List.map (assign_tree state) proto_dies in
  let table = { entries = List.rev state.entries } in
  (abbrev_dies, table)

let abbrev_code die = die.abbrev_code

let proto_die die = die.proto_die

let children _die =
  (* Would need to store processed children - simplified for now *)
  []

let print_abbrev_entry ppf entry =
  Format.fprintf ppf "@[<v 2>Abbrev %d:" entry.code;
  Format.fprintf ppf "@,Tag: %s" (Dwarf_tag.to_string entry.tag);
  Format.fprintf ppf "@,Children: %s" (if entry.has_children then "yes" else "no");
  if entry.attributes <> [] then begin
    Format.fprintf ppf "@,Attributes:";
    List.iter (fun (attr, form) ->
      Format.fprintf ppf "@,  %s (%s)"
        (Dwarf_attributes.to_string attr)
        (Dwarf_form.to_string form)
    ) entry.attributes
  end;
  Format.fprintf ppf "@]"

let print_abbrev_table ppf (table : abbrev_table) =
  Format.fprintf ppf "@[<v>Abbreviation Table:";
  List.iter (fun entry ->
    Format.fprintf ppf "@,@,";
    print_abbrev_entry ppf entry
  ) table.entries;
  Format.fprintf ppf "@]"

let print ppf die =
  Format.fprintf ppf "@[<v 2>[Abbrev %d] %a@]"
    die.abbrev_code
    Proto_die.print die.proto_die
