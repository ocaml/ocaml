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

(** *)

type linkage_name = string

let linkage_name s = s
let string_of_linkage_name s = s


(** *)

type compilation_unit =
  { id: string;
    linkage_name: linkage_name }

module Compilation_unit = struct

  type t = compilation_unit

  let create id linkage_name =
    { id; linkage_name }

  let get_persistent_ident cu = Ident.create_persistent cu.id
  let get_linkage_name cu = cu.linkage_name

  (* multiple units can have the same id, if they are in different
     pack. To distinguish we also keep the linkage name which contains
     the pack name *)
  let compare v1 v2 =
    let c = String.compare v1.id v2.id in
    if c = 0
    then String.compare v1.linkage_name v2.linkage_name
    else c

  let equal x y = compare x y = 0

  let print ppf x = Format.pp_print_string ppf x.id
  let output oc x = output_string oc x.id
  let hash x = Hashtbl.hash x.id

  let name x = x.id

end

module CompilationUnitSet = ExtSet(Compilation_unit)
module CompilationUnitMap = ExtMap(Compilation_unit)
module CompilationUnitTbl = ExtHashtbl(Compilation_unit)

(** *)

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

module SymbolSet = ExtSet(Printers)
module SymbolMap = ExtMap(Printers)
module SymbolTbl = ExtHashtbl(Printers)
