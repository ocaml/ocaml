(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

type t = {
  id : Ident.t;
  for_pack_prefix : string option;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ id = id1; for_pack_prefix = for_pack_prefix1; hash = hash1; } as t1)
        ({ id = id2; for_pack_prefix = for_pack_prefix2; hash = hash2; } as t2)
        =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare t1.hash t2.hash in
      if c <> 0 then c
      else
        let c = String.compare (Ident.name t1.id) (Ident.name t2.id) in
        if c <> 0 then c
        else
          Misc.Stdlib.Option.compare String.compare
            t1.for_pack_prefix t2.for_pack_prefix

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf t = Format.pp_print_string ppf (Ident.name t.id)
  let output oc t = output_string oc (Ident.name t.id)

  let hash t = t.hash
end)

let create ~for_pack_prefix name =
  let id = Ident.create_persistent name in
  if not (Ident.persistent id) then begin
    Misc.fatal_error "Compilation_unit.create with non-persistent Ident.t"
  end;
  begin match for_pack_prefix with
  | Some "" -> Misc.fatal_error "[for_pack_prefix] is [Some] but empty"
  | None | Some _ -> ()
  end;
  { id;
    for_pack_prefix;
    hash = Hashtbl.hash (Ident.name id, for_pack_prefix);
  }

let name t = Ident.name t.id
let for_pack_prefix t = t.for_pack_prefix

(* The string here must match the symbol names in the .S files in runtime/. *)
let runtime_and_external_libs = create "_system"

let startup = create "_startup"
let shared_startup = create "_shared_startup"

let is_startup_or_shared_startup t =
  equal t startup || equal t shared_startup

let predefined_exn = create "_predef_exn"

let current = ref None

let is_current_exn arg =
  match !current with
  | None -> Misc.fatal_error "Current compilation unit is not set"
  | Some cur -> equal cur arg

let set_current t = current := Some t

let get_current () = !current

let get_current_exn () =
  match !current with
  | Some current -> current
  | None -> Misc.fatal_error "Current compilation unit is not set"

let get_current_id_exn () = get_persistent_ident (get_current_exn ())
