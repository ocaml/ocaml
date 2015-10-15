(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module T = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    label : Linkage_name.t;
  }

  (* CR mshinwell for pchambart: I tried to rewrite the second sentence of
     this comment, but it still isn't great.  Can you try again?  Note that
     there's something about uniqueness in the comment in symbol.mli too. *)
  (** Labels are unique, so comparing them is sufficient.  Ignoring the
      compilation unit may also uncover bugs. *)
  let compare t1 t2 = Linkage_name.compare t1.label t2.label

  let output chan t = Linkage_name.output chan t.label
  let hash t = Hashtbl.hash t.label
  let equal t1 t2 = t1.label = t2.label

  let print ppf t =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print t.compilation_unit
      Linkage_name.print t.label
end

include T
include Ext_types.Identifiable.Make (T)

let create compilation_unit label =
  let unit_linkage_name =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  let label =
    Linkage_name.create (unit_linkage_name ^ "__" ^ (Linkage_name.to_string label))
  in
  { compilation_unit; label; }

let unsafe_create compilation_unit label =
  { compilation_unit; label; }

let import_for_pack ~pack:compilation_unit symbol =
  { compilation_unit; label = symbol.label; }

let compilation_unit t = t.compilation_unit
let label t = t.label

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no symbol>"
  | Some t -> print ppf t

let compare_lists l1 l2 = Misc.compare_lists compare l1 l2
