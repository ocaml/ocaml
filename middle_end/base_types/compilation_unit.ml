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
    id : Ident.t;
    linkage_name : Linkage_name.t;
  }

  (* Multiple units can have the same [id] if they come from different packs.
     To distinguish these we also keep the linkage name, which contains the
     name of the pack. *)
  let compare v1 v2 =
    let v1_id = Ident.name v1.id in
    let v2_id = Ident.name v2.id in
    let c = String.compare v1_id v2_id in
    if c = 0
    then Linkage_name.compare v1.linkage_name v2.linkage_name
    else c

  let equal x y = compare x y = 0

  let print ppf x = Format.pp_print_string ppf (Ident.name x.id)
  let output oc x = output_string oc (Ident.name x.id)
  let hash x = Hashtbl.hash (Ident.name x.id)
end

include T
include Ext_types.Identifiable.Make (T)

let create id linkage_name =
  if not (Ident.persistent id) then begin
    Misc.fatal_error "Compilation_unit.create with non-persistent Ident.t"
  end;
  { id; linkage_name }

let get_persistent_ident cu = cu.id
let get_linkage_name cu = cu.linkage_name

let current = ref None
let set_current t = current := Some t
let get_current () = !current
let get_current_exn () =
  match !current with
  | Some current -> current
  | None -> Misc.fatal_error "Compilation_unit.get_current_exn"
let get_current_id_exn () = get_persistent_ident (get_current_exn ())
