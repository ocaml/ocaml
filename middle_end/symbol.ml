(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types

type linkage_name = string

let linkage_name s = s
let string_of_linkage_name s = s

type compilation_unit = {
  id: Ident.t;
  linkage_name: linkage_name;
}

module Compilation_unit = struct
  module T = struct
    type t = compilation_unit
    (* multiple units can have the same id, if they are in different
       pack. To distinguish we also keep the linkage name which contains
       the pack name *)
    let compare v1 v2 =
      let v1_id = Ident.name v1.id in
      let v2_id = Ident.name v2.id in
      let c = String.compare v1_id v2_id in
      if c = 0
      then String.compare v1.linkage_name v2.linkage_name
      else c

    let equal x y = compare x y = 0

    let print ppf x = Format.pp_print_string ppf (Ident.name x.id)
    let output oc x = output_string oc (Ident.name x.id)
    let hash x = Hashtbl.hash (Ident.name x.id)
  end

  include T
  include Identifiable.Make (T)

  let create id linkage_name =
    { id; linkage_name }

  let get_persistent_ident cu = Ident.create_persistent (Ident.name cu.id)
  let get_linkage_name cu = cu.linkage_name

  let current = ref None
  let set_current t = current := Some t
  let get_current () = !current
  let get_current_exn () =
    match !current with
    | Some current -> current
    | None -> Misc.fatal_error "Compilation_unit.get_current_exn"
  let get_current_id_exn () = (get_current_exn ()).id
end

type symbol = { sym_unit : compilation_unit; sym_label : linkage_name }

module Printers = struct
  type t = symbol = { sym_unit : compilation_unit; sym_label : linkage_name }
  let compare s1 s2 = String.compare s1.sym_label s2.sym_label
  (** Labels are unique, so comparing them is sufficient. It also could
      uncover bugs to consider same labels from different modules equal *)
  let output c s = output_string c s.sym_label
  let hash s = Hashtbl.hash s.sym_label
  let equal s1 s2 = s1.sym_label = s2.sym_label
  let print ppf s =
    Format.fprintf ppf "%a - %s" Compilation_unit.print s.sym_unit s.sym_label
end

include Printers

module Symbol_Identifiable = struct
  include Printers
  include Identifiable.Make(Printers)
end

module SymbolSet = Symbol_Identifiable.Set
module SymbolMap = Symbol_Identifiable.Map
module SymbolTbl = Symbol_Identifiable.Tbl

module Innerid = Id(struct end)
module ExportId = UnitId(Innerid)(Compilation_unit)
